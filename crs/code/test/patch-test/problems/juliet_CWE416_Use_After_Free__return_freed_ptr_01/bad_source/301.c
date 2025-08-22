#include "std_testcase.h"
static char * helperstuff(char * aString)
{
    size_t i = 0;
    size_t j;
    char * reversedString = NULL;
    if (aString != NULL)
    {
        i = strlen(aString);
        reversedString = (char *) malloc(i+1);
        if (reversedString == NULL) {exit(-1);}
        for (j = 0; j < i; j++)
        {
            reversedString[j] = aString[i-j-1];
        }
        reversedString[i] = '\0';
        free(reversedString);
        return reversedString;
    }
    else
    {
        return NULL;
    }
}
static char * helperGood(char * aString)
{
    size_t i = 0;
    size_t j;
    char * reversedString = NULL;
    if (aString != NULL)
    {
        i = strlen(aString);
        reversedString = (char *) malloc(i+1);
        if (reversedString == NULL) {exit(-1);}
        for (j = 0; j < i; j++)
        {
            reversedString[j] = aString[i-j-1];
        }
        reversedString[i] = '\0';
        return reversedString;
    }
    else
    {
        return NULL;
    }
}
#ifndef OMITstuff
void foofooreturn_freed_ptr_01_stuff()
{
    {
        char * reversedString = helperstuff("stuffSink");
        printLine(reversedString);
    }
}
#endif
#ifdef INCLUDEMAIN
int main(int argc, char * argv[])
{
    srand( (unsigned)time(NULL) );
#ifndef OMITstuff
    printLine("Calling stuff()...");
    foofooreturn_freed_ptr_01_stuff();
    printLine("Finished stuff()");
#endif
    return 0;
}
#endif
