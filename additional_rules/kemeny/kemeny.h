#ifndef KEMENY_HEADER
#define KEMENY_HEADER

#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <climits>

using namespace std;

typedef vector<vector<wstring>> preference_profile;

vector<set<wstring>> compute_all_subsets_of_size(int size, vector<wstring> supervector);
int compute_KT_distance(vector<vector<wstring>> profile, vector<wstring> ranking);
bool compute_kemeny_ranking(preference_profile &profile, vector<wstring> &out_winners);

#endif