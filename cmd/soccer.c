#include <windows.h>
#include <winbase.h>
#include <windef.h>
#include <stdio.h>

typedef int (*ExecScript)(char *, char ***, int *);

int loadSoccerLib(*ExecScript out_proc)
{
    HINSTANCE testLibrary = LoadLibrary("libsoccer.dll");
    if (testLibrary)
    {
        out_ptoc = &(ExecScript)GetProcAddress(testLibrary, "ExecScript");
        return testLibrary;
    }
    else
    {
        return testLibrary;
    }
}

int unLoadLib(HINSTANCE a_lib)
{
    FreeLibrary(a_lib);
}

int loadScriptFromFile(char *filename; char **out_script)
{
}

int main(int argc, char *argv[])
{
    int status = 0;
    ExecScript _ExecScript;

    HINSTANCE lib = loadSoccerLib(&_ExecScript);
    if (lib)
    {
        if (argc > 1)
        {
            char *scriptFilename = argv[1];
            char *script;
            int result = loadScriptFromFile(scriptFilename, &script);
            if (result)
            {
                char **out_array = NULL;
                int out_length = 0;
                _ExecScript(a_script, &out_array, &out_length);
            }
            else
            {
                printf("%s","Can't load script from a file");
            }
        }
        else
        {
            printf("%s", "No script specified");
        }
        unLoadLib(lib);
    }
    else
    {
        printf("%s", "libsoccer not found");
    }
    getchar();
}