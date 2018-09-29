#include <iostream>
#include <vector>
#include <map>
#include <functional>
using namespace std;

typedef vector<vector<string>> preference_profile;

struct strvect_comparator
{
    bool operator()(const std::string &a, const std::string &b) const
    {
        return std::equal(a.begin(), a.end(), b.begin(), [](const char &l, const char &r) {
            return (::toupper(l) == ::toupper(r));
        });
    }
};

bool compute_kemeny_ranking(preference_profile &profile, vector<string> &out_winners)
{
    //build a list of alternatives
    vector<string> alternatives(profile.at(0).size);
    for (unsigned int i = 0; i < alternatives.size(); i++)
    {
        alternatives.at(i) = profile.at(0).at(i);
    }
    
    //create a data-table
    map<vector<string>, vector<string>, strvect_comparator> data_table;

    //TODO: test it

    return true;
}

int main(int argc, char const *argv[])
{
    int n = 3; //number of voters
    int m = 3; //number of alternatives

    preference_profile profile(n, vector<string>(m));
    profile.at(0).at(0) = "a";
    profile.at(0).at(1) = "b";
    profile.at(0).at(2) = "c";

    profile.at(1).at(0) = "a";
    profile.at(1).at(1) = "b";
    profile.at(1).at(2) = "c";

    profile.at(2).at(0) = "c";
    profile.at(2).at(1) = "b";
    profile.at(2).at(2) = "a";

    vector<string> winners;

    compute_kemeny_ranking(profile, winners);

    for (string winner : winners)
    {
        cout << winner << endl;
    }

    return 0;
}
