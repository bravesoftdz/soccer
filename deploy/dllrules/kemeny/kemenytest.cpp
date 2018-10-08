#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <climits>

#include "kemeny.h"

using namespace std;

/*
Test program for Kemeny
*/

int main(int argc, char const *argv[])
{
    /*
    Output:
    |a|b|
    |a|c|
    |a|d|
    |b|c|
    |b|d|
    |c|d|
    */
    cout << "Test 1: subsets" << endl;
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
    cout << "Test 1: subsets finised" << endl
         << endl;

    /*
    Output:
    |a|b|c|
    |a|b|d|
    |a|c|d|
    |b|c|d|
    */
    cout << "Test 2: subsets2" << endl;
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
    cout << "Test 2: subsets2 finised" << endl
         << endl;

    /*
    Output:
    |a|b|c|d|
    */
    cout << "Test 3: subsets3" << endl;
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
    cout << "Test 3: subsets3 finised" << endl
         << endl;

    //Kemeny

    /*
    Output:
    a b c
    */
    cout << "Test 4: Kemeny 1 voter" << endl;

    int n = 1; //number of voters
    int m = 3; //number of alternatives

    preference_profile profile(n, vector<string>(m));
    profile.at(0).at(0) = "a";
    profile.at(0).at(1) = "b";
    profile.at(0).at(2) = "c";

    vector<string> winners;

    compute_kemeny_ranking(profile, winners);

    for (string winner : winners)
    {
        cout << winner << " ";
    }
    cout << endl;
    cout << "Test 4: Kemeny 1 voter finished" << endl
         << endl;

    /*
    Output:
    c a b
    */
    cout << "Test 5: Kemeny Condorcet paradox" << endl;

    n = 3; //number of voters
    m = 3; //number of alternatives

    preference_profile profile2(n, vector<string>(m));
    profile2.at(0).at(0) = "a";
    profile2.at(0).at(1) = "b";
    profile2.at(0).at(2) = "c";

    profile2.at(1).at(0) = "c";
    profile2.at(1).at(1) = "a";
    profile2.at(1).at(2) = "b";

    profile2.at(2).at(0) = "c";
    profile2.at(2).at(1) = "b";
    profile2.at(2).at(2) = "a";

    vector<string> winners2;

    compute_kemeny_ranking(profile2, winners2);

    for (string winner : winners2)
    {
        cout << winner << " ";
    }
    cout << endl;
    cout << "Test 5: Kemeny Condorcet paradox finished" << endl
         << endl;

    /*
    Output:
    d b c a
    */
    cout << "Test 6: Kemeny normal" << endl;

    n = 10; //number of voters
    m = 4;  //number of alternatives

    preference_profile profile3(n, vector<string>(m));
    profile3.at(0).at(0) = "d";
    profile3.at(0).at(1) = "b";
    profile3.at(0).at(2) = "c";
    profile3.at(0).at(3) = "a";

    profile3.at(1).at(0) = "d";
    profile3.at(1).at(1) = "b";
    profile3.at(1).at(2) = "c";
    profile3.at(1).at(3) = "a";

    profile3.at(2).at(0) = "d";
    profile3.at(2).at(1) = "b";
    profile3.at(2).at(2) = "c";
    profile3.at(2).at(3) = "a";

    profile3.at(3).at(0) = "a";
    profile3.at(3).at(1) = "b";
    profile3.at(3).at(2) = "d";
    profile3.at(3).at(3) = "c";

    profile3.at(4).at(0) = "a";
    profile3.at(4).at(1) = "b";
    profile3.at(4).at(2) = "d";
    profile3.at(4).at(3) = "c";

    profile3.at(5).at(0) = "a";
    profile3.at(5).at(1) = "b";
    profile3.at(5).at(2) = "d";
    profile3.at(5).at(3) = "c";

    profile3.at(6).at(0) = "d";
    profile3.at(6).at(1) = "c";
    profile3.at(6).at(2) = "b";
    profile3.at(6).at(3) = "a";

    profile3.at(7).at(0) = "d";
    profile3.at(7).at(1) = "c";
    profile3.at(7).at(2) = "b";
    profile3.at(7).at(3) = "a";

    profile3.at(8).at(0) = "d";
    profile3.at(8).at(1) = "c";
    profile3.at(8).at(2) = "b";
    profile3.at(8).at(3) = "a";

    profile3.at(9).at(0) = "c";
    profile3.at(9).at(1) = "d";
    profile3.at(9).at(2) = "b";
    profile3.at(9).at(3) = "a";

    vector<string> winners3;

    compute_kemeny_ranking(profile3, winners3);

    for (string winner : winners3)
    {
        cout << winner << " ";
    }
    cout << endl;
    cout << "Test 6: Kemeny normal finished" << endl
         << endl;

    return 0;
}
