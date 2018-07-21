#include <windows.h>
#include <winbase.h>
#include <windef.h>
#include <stdio.h>

typedef int (*ExecScript)(char*, char***, int*);

void main()
{
    int status = 0;
    ExecScript _ExecScript;
    HINSTANCE testLibrary = LoadLibrary("libsoccer.dll");

    if (testLibrary)
    {
        _ExecScript = (ExecScript)GetProcAddress(testLibrary, "ExecScript");
        if (_ExecScript)
        {
            char *a_script = "Hello,222";
            char** out_array = NULL;
            int out_length = 0;
            _ExecScript(a_script, &out_array, &out_length);
            printf("Executed, length: %d\n", out_length);
            for (int i = 0; i < out_length; i++){
                printf("%s\n", out_array[i]);
            }
        }
        FreeLibrary(testLibrary);
    }
    getchar();
}