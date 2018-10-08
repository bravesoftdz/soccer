#ifndef KEMENY_HEADER
#define KEMENY_HEADER

#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <climits>

using namespace std;

typedef vector<vector<string>> preference_profile;

vector<set<string>> compute_all_subsets_of_size(int size, vector<string> supervector);
int compute_KT_distance(vector<vector<string>> profile, vector<string> ranking);
bool compute_kemeny_ranking(preference_profile &profile, vector<string> &out_winners);

#endif