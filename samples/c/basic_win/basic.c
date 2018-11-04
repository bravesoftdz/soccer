#include <windows.h>
#include <winbase.h>
#include <windef.h>
#include <stdio.h>
#include <wchar.h>

typedef wchar_t** (*ExecScript)(wchar_t*, int*);
typedef void (*FreeSoccerPtr)(wchar_t***, int);

int main()
{
    ExecScript _ExecScript;
    FreeSoccerPtr _FreeSoccerPtr;
    HINSTANCE testLibrary = LoadLibrary("libsoccer.dll");

    if (testLibrary)
    {
        _ExecScript = (ExecScript)GetProcAddress(testLibrary, "ExecScript");
        _FreeSoccerPtr = (FreeSoccerPtr)GetProcAddress(testLibrary, "FreeSoccerPtr");
        if (_ExecScript)
        {
            wchar_t *a_script = L"START[voting] IMPORT[plurality] VOTE(a->b) DECIDE!";
            int out_length = 0;
            wchar_t** out_array = _ExecScript(a_script, &out_length);
            printf("Executed, length: %d\n", out_length);
            for (int i = 0; i < out_length; i++){
                wprintf(L"%ls\n", out_array[i]);
            }
            _FreeSoccerPtr(&out_array, out_length);
        }
        FreeLibrary(testLibrary);
    }
    getchar();
}