#include <windows.h>
#include <winbase.h>
#include <windef.h>
#include <stdio.h>
#include <stdbool.h>

bool DEBUG = true;

typedef int (*ExecScript)(char *, char ***, int *);

int loadSoccerLib(*ExecScript out_proc)
{
    HINSTANCE testLibrary;
    if (DEBUG)
    {
        testLibrary = LoadLibrary("..\\libsoccer\\_dll\\Win32\\Debug\\libsoccer.dll");
    }
    else
    {
        testLibrary = LoadLibrary("libsoccer.dll");
    }
    if (testLibrary)
    {
        out_proc = &(ExecScript)GetProcAddress(testLibrary, "ExecScript");
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

int loadScriptFromFile(char *filename, char **out_script)
{
    FILE *script = fopen(filename, "r");

    fseek(script, 0, SEEK_END);
    long fsize = ftell(script);
    fseek(script, 0, SEEK_SET);

    *out_script = malloc(fsize + 1);
    fread(*out_script, fsize, 1, script);
    fclose(script);

    out_script[fsize] = 0;
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
            printf("%s", &script);
            if (result)
            {
                char **out_array = NULL;
                int out_length = 0;
                _ExecScript(script, &out_array, &out_length);
            }
            else
            {
                printf("%s", "Can't load script from a file");
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