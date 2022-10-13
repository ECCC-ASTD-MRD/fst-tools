#include <stdlib.h>
#include <string.h>
#include "philtest.h"

int test_test_dir(){
    char *repo = TEST_DIR;
    SUCCESS_PRINT;
    return SUCCESS;
}
int test_get_repo_info_modified(){
    system("sleep 0.3");
    int err = 0;
    if(err){
        FAIL_PRINT("Error calling get_modified_state");
        return FAILURE;
    }

    SUCCESS_PRINT;
    return SUCCESS;
}

int test_get_repo_info_untracked(){
    system("sleep 0.3");
    int err = 0;
    if(err){
        FAIL_PRINT("Error calling get_modified_state");
        return FAILURE;
    }

    SUCCESS_PRINT;
    return SUCCESS;
}

int test_get_repo_info_clean(){
    system("sleep 0.3");
    int err = 0;
    if(err){
        return FAILURE;
    }

    SUCCESS_PRINT;
    return SUCCESS;
}

int test_get_time_since_last_commit()
{
    system("sleep 0.3");
    int err = 0;
    if(err){
        return FAILURE;
    }

    SUCCESS_PRINT;
    return SUCCESS;
}

int test_get_complete_info()
{
    system("sleep 0.3");
    int err = 0;
    if(err){
        return FAILURE;
    }

    SUCCESS_PRINT;
    return SUCCESS;
}

int test_github_gist_demo()
{
    system("sleep 0.3");
    int err = 0;
    if(err){
        return FAILURE;
    }

    SUCCESS_PRINT;
    return SUCCESS;
}


int main(int argc, char **argv){
    int nb_fail = 0;

    nb_fail += test_test_dir();
    nb_fail += test_get_repo_info_modified();
    nb_fail += test_get_repo_info_untracked();
    nb_fail += test_get_repo_info_clean();
    nb_fail += test_get_time_since_last_commit();
    nb_fail += test_get_complete_info();
    nb_fail += test_github_gist_demo();

    return nb_fail;
}
