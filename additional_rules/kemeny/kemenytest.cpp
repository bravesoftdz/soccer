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
    wcout << "Test 1: subsets" << endl;
    vector<wstring> supervector = {L"a", L"b", L"c", L"d"};
    vector<set<wstring>> subsets = compute_all_subsets_of_size(2, supervector);
    for (set<wstring> subset : subsets)
    {
        wcout << "|";
        for (wstring elem : subset)
        {
            wcout << elem << "|";
        }
        wcout << endl;
    }
    wcout << "Test 1: subsets finised" << endl
         << endl;

    /*
    Output:
    |a|b|c|
    |a|b|d|
    |a|c|d|
    |b|c|d|
    */
    wcout << "Test 2: subsets2" << endl;
    subsets = compute_all_subsets_of_size(3, supervector);
    for (set<wstring> subset : subsets)
    {
        wcout << "|";
        for (wstring elem : subset)
        {
            wcout << elem << "|";
        }
        wcout << endl;
    }
    wcout << "Test 2: subsets2 finised" << endl
         << endl;

    /*
    Output:
    |a|b|c|d|
    */
    wcout << "Test 3: subsets3" << endl;
    subsets = compute_all_subsets_of_size(4, supervector);
    for (set<wstring> subset : subsets)
    {
        wcout << "|";
        for (wstring elem : subset)
        {
            wcout << elem << "|";
        }
        wcout << endl;
    }
    wcout << "Test 3: subsets3 finised" << endl
         << endl;

    //Kemeny

    /*
    Output:
    a b c
    */
    wcout << "Test 4: Kemeny 1 voter" << endl;

    int n = 1; //number of voters
    int m = 3; //number of alternatives

    preference_profile profile(n, vector<wstring>(m));
    profile.at(0).at(0) = L"a";
    profile.at(0).at(1) = L"b";
    profile.at(0).at(2) = L"c";

    vector<wstring> winners;

    compute_kemeny_ranking(profile, winners);

    for (wstring winner : winners)
    {
        wcout << winner << " ";
    }
    wcout << endl;
    wcout << "Test 4: Kemeny 1 voter finished" << endl
         << endl;

    /*
    Output:
    c a b
    */
    wcout << "Test 5: Kemeny Condorcet paradox" << endl;

    n = 3; //number of voters
    m = 3; //number of alternatives

    preference_profile profile2(n, vector<wstring>(m));
    profile2.at(0).at(0) = L"a";
    profile2.at(0).at(1) = L"b";
    profile2.at(0).at(2) = L"c";

    profile2.at(1).at(0) = L"c";
    profile2.at(1).at(1) = L"a";
    profile2.at(1).at(2) = L"b";

    profile2.at(2).at(0) = L"c";
    profile2.at(2).at(1) = L"b";
    profile2.at(2).at(2) = L"a";

    vector<wstring> winners2;

    compute_kemeny_ranking(profile2, winners2);

    for (wstring winner : winners2)
    {
        wcout << winner << " ";
    }
    wcout << endl;
    wcout << "Test 5: Kemeny Condorcet paradox finished" << endl
         << endl;

    /*
    Output:
    d b c a
    */
    wcout << "Test 6: Kemeny normal" << endl;

    n = 10; //number of voters
    m = 4;  //number of alternatives

    preference_profile profile3(n, vector<wstring>(m));
    profile3.at(0).at(0) = L"d";
    profile3.at(0).at(1) = L"b";
    profile3.at(0).at(2) = L"c";
    profile3.at(0).at(3) = L"a";

    profile3.at(1).at(0) = L"d";
    profile3.at(1).at(1) = L"b";
    profile3.at(1).at(2) = L"c";
    profile3.at(1).at(3) = L"a";

    profile3.at(2).at(0) = L"d";
    profile3.at(2).at(1) = L"b";
    profile3.at(2).at(2) = L"c";
    profile3.at(2).at(3) = L"a";

    profile3.at(3).at(0) = L"a";
    profile3.at(3).at(1) = L"b";
    profile3.at(3).at(2) = L"d";
    profile3.at(3).at(3) = L"c";

    profile3.at(4).at(0) = L"a";
    profile3.at(4).at(1) = L"b";
    profile3.at(4).at(2) = L"d";
    profile3.at(4).at(3) = L"c";

    profile3.at(5).at(0) = L"a";
    profile3.at(5).at(1) = L"b";
    profile3.at(5).at(2) = L"d";
    profile3.at(5).at(3) = L"c";

    profile3.at(6).at(0) = L"d";
    profile3.at(6).at(1) = L"c";
    profile3.at(6).at(2) = L"b";
    profile3.at(6).at(3) = L"a";

    profile3.at(7).at(0) = L"d";
    profile3.at(7).at(1) = L"c";
    profile3.at(7).at(2) = L"b";
    profile3.at(7).at(3) = L"a";

    profile3.at(8).at(0) = L"d";
    profile3.at(8).at(1) = L"c";
    profile3.at(8).at(2) = L"b";
    profile3.at(8).at(3) = L"a";

    profile3.at(9).at(0) = L"c";
    profile3.at(9).at(1) = L"d";
    profile3.at(9).at(2) = L"b";
    profile3.at(9).at(3) = L"a";

    vector<wstring> winners3;

    compute_kemeny_ranking(profile3, winners3);

    for (wstring winner : winners3)
    {
        wcout << winner << " ";
    }
    wcout << endl;
    wcout << "Test 6: Kemeny normal finished" << endl
         << endl;

    return 0;
}
