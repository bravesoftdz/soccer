#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <climits>
using namespace std;

typedef vector<vector<string>> preference_profile;

vector<set<string>> compute_all_subsets_of_size(int size, vector<string> supervector)
{
    vector<set<string>> result;
    vector<unsigned int> pointers;
    for (int i = 0; i < size; i++)
    {
        if (i == 0)
        {
            pointers.push_back(0);
        }
        else
        {
            pointers.push_back(1);
        }
    }
    bool finished = false;
    while (!finished)
    {
        set<string> l_set;
        int sum = 0;
        for (int pointer : pointers)
        {
            sum += pointer;
            l_set.insert(supervector.at(sum));
        }
        result.push_back(l_set);
        for (int i = pointers.size() - 1; i >= 0; i--)
        {
            pointers.at(i) += 1;
            unsigned int sum_i = 0;
            for (int j = i; j >= 0; j--)
            {
                sum_i += pointers.at(j);
            }
            unsigned int border = supervector.size() - (pointers.size() - 1 - i);
            if (sum_i >= border)
                {
                    pointers.at(i) = 1;
                    if (i == 0)
                    {
                        finished = true;
                    }
                }
            else
            {
                break;
            }
        }
    }
    return result;
}

int compute_KT_distance(vector<vector<string>> profile, vector<string> ranking)
{
    int kt_dist = 0;
    for (unsigned int i = 0; i < ranking.size(); i++)
    {
        string alt_1 = ranking.at(i);
        for (unsigned int j = i + 1; j < ranking.size(); j++)
        {
            string alt_2 = ranking.at(j);
            for (vector<string> voter : profile)
            {
                int index_1, index_2;
                for (unsigned int k = 0; k < voter.size(); k++)
                {
                    if (voter.at(k) == alt_1)
                    {
                        index_1 = k;
                    }
                    if (voter.at(k) == alt_2)
                    {
                        index_2 = k;
                    }
                }
                if (index_2 > index_1)
                {
                    kt_dist += 1;
                }
            }
        }
    }
    return kt_dist;
}

bool compute_kemeny_ranking(preference_profile &profile, vector<string> &out_winners)
{
    //build a list of alternatives
    vector<string> alternatives(profile.at(0).size());
    for (unsigned int i = 0; i < alternatives.size(); i++)
    {
        alternatives.at(i) = profile.at(0).at(i);
    }

    //create a data-table
    map<set<string>, vector<string>> data_table;

    //Iterate over set sizes
    for (unsigned int i = 2; i <= alternatives.size(); i++)
    {
        vector<set<string>> subsets = compute_all_subsets_of_size(i, alternatives);
        if (i == 2)
        {
            //Compute KT distances for pairs (base step)
            for (set<string> subset : subsets)
            {
                std::vector<string> rank(subset.begin(), subset.end());
                int kt_1 = compute_KT_distance(profile, rank);
                std::reverse(rank.begin(), rank.end());
                int kt_2 = compute_KT_distance(profile, rank);
                if (kt_1 < kt_2)
                {
                    std::reverse(rank.begin(), rank.end());
                    data_table[subset] = rank;
                }
                else
                {
                    data_table[subset] = rank;
                }
            }
        }
        else
        {
            //Compute KT distances for subsets with 3 and more elements using previous subsets
            for (set<string> subset : subsets)
            {
                //Try to set a particular alternative as a first one
                int best_kt_dist = INT_MAX;
                vector<string> best_rank;
                for (string first_alternative : subset)
                {
                    //Copy set and remove the selected alternative
                    set<string> subsubset = subset;
                    subsubset.erase(first_alternative);
                    //find a best ranking for a subset without selected alternative
                    vector<string> subsubranking = data_table[subsubset];
                    //add selected alternative as the first in a new ranking
                    vector<string> ranking;
                    ranking.push_back(first_alternative);
                    ranking.insert(ranking.end(), subsubranking.begin(), subsubranking.end());
                    int kt_dist = compute_kemeny_ranking(profile, ranking);
                    //if it's better for a current subset, then save it
                    if (kt_dist < best_kt_dist)
                    {
                        best_kt_dist = kt_dist;
                        best_rank = ranking;
                    }
                }
                //Save the best ranking
                data_table[subset] = best_rank;
            }
        }
    }
    //Extract the best ranking
    set<string> alternatives_set(alternatives.begin(), alternatives.end());
    vector<string> kemeny_ranking = data_table[alternatives_set];
    out_winners.push_back(kemeny_ranking.at(0));
    return true;
}

int main(int argc, char const *argv[])
{
    //subsets
    vector<string> supervector = {"a", "b", "c", "d"};
    vector<set<string>> subsets = compute_all_subsets_of_size(2, supervector);
    for (set<string> subset : subsets)
    {
        cout << "|";
        for (string elem : subset)
        {
            cout << elem << "|";
        }
        cout << endl;
    }

    subsets = compute_all_subsets_of_size(3, supervector);
    for (set<string> subset : subsets)
    {
        cout << "|";
        for (string elem : subset)
        {
            cout << elem << "|";
        }
        cout << endl;
    }

    subsets = compute_all_subsets_of_size(4, supervector);
    for (set<string> subset : subsets)
    {
        cout << "|";
        for (string elem : subset)
        {
            cout << elem << "|";
        }
        cout << endl;
    }

    //     //Kemeny
    //     int n = 1; //number of voters
    //     int m = 3; //number of alternatives

    //     preference_profile profile(n, vector<string>(m));
    //     profile.at(0).at(0) = "a";
    //     profile.at(0).at(1) = "b";
    //     profile.at(0).at(2) = "c";

    //     vector<string> winners;

    //     compute_kemeny_ranking(profile, winners);

    //     for (string winner : winners)
    //     {
    //         cout << winner << endl;
    //     }

    return 0;
}
