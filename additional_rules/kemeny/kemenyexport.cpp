#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <climits>
#include <cwchar>

#include "kemeny.h"

using namespace std;

typedef struct
{
	int alternatives_count;
	int voters_count;
	bool complete;
} soccerVotingPreferencesProperties;

std::wstring str(L"kemeny");

extern "C"
{
	__declspec(dllexport) const wchar_t *getName()
	{
		return str.c_str();
	}
	__declspec(dllexport) int executeOn(wchar_t ***a_profile, soccerVotingPreferencesProperties a_properties, wchar_t ***out_winners, int64_t *winnersLength)
	{
		//Check input
		if (!a_properties.complete)
			return 0;

		//Convert
		cout << "Convert" << endl;
		vector<vector<std::wstring>> profile(a_properties.voters_count, vector<wstring>(a_properties.alternatives_count));
		for (int i = 0; i < a_properties.voters_count; i++)
		{
			for (int j = 0; j < a_properties.alternatives_count; j++)
			{
				std::wstring str = a_profile[i][j];
				profile.at(i).at(j) = str;
			}
		}

		cout << "Call" << endl;
		vector<wstring> winners = {L"alt2"};
		int res = 1;
		// int res = compute_kemeny_ranking(profile, winners);

		cout << "Back" << endl;
		out_winners = (wchar_t ***)malloc(sizeof(wchar_t **));
		(*out_winners) = (wchar_t **)malloc(sizeof(wchar_t *));
		(*out_winners)[0] = (wchar_t *)malloc(sizeof(wchar_t) * (winners.size() + 1));
		wcscpy((*out_winners)[0], winners.at(0).c_str());

		cout << "Return" << endl;
		cout << "Result:" << endl;
		cout << out_winners << " " << (*out_winners) << " " << (*out_winners)[0] << endl;

		winnersLength = (int64_t *)malloc(sizeof(int64_t));
		(*winnersLength) = 1;

		return res;
	}
}