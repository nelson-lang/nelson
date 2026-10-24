#include "fmi4c_utils.h"
#include "fmi4c_common.h"

#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
#include <windows.h>
#include <direct.h>
#else
// For MinGW or GCC
#include <dirent.h>
#include <sys/stat.h>
#endif

#ifdef WIN32
#define FILE_PATH_SEPARATOR '\\'
#else
#include <unistd.h>
#define FILE_PATH_SEPARATOR '/'
#endif

void
rememberPointer(fmuHandle* fmu, void* ptr)
{
    fmu->numAllocatedPointers++;
    fmu->allocatedPointers
        = realloc(fmu->allocatedPointers, fmu->numAllocatedPointers * sizeof(void*));
    fmu->allocatedPointers[fmu->numAllocatedPointers - 1] = ptr;
}

void*
mallocAndRememberPointer(fmuHandle* fmu, size_t size)
{
    void* ptr = malloc(size);
    rememberPointer(fmu, ptr);
    return ptr;
}

void*
reallocAndRememberPointer(fmuHandle* fmu, void* org, size_t size)
{
    int i = 0;
    while (i < fmu->numAllocatedPointers && org != fmu->allocatedPointers[i]) {
        ++i;
    }
    void* ptr = realloc(org, size);
    if (i < fmu->numAllocatedPointers) {
        fmu->allocatedPointers[i] = ptr;
    } else {
        rememberPointer(fmu, ptr);
    }
    return ptr;
}

char*
duplicateAndRememberString(fmuHandle* fmu, const char* str)
{
    char* ret = _strdup(str);
    rememberPointer(fmu, (void*)ret);
    return ret;
}

//! @brief Concatenates model name and function name into "modelName_functionName" (for FMI 1)
//! @param modelName FMU model name
//! @param functionName Function name
//! @param concatBuffer A buffer in which to concatenate modelName_functionName, this buffer must be
//! FILENAME_MAX big
//! @returns Full (concatenated) function name
const char*
getFunctionName(const char* modelName, const char* functionName, char* concatBuffer)
{
    if (modelName == NULL || strlen(modelName) == 0) {
        // Do not change function name if model name is empty, in this case there is no need to use
        // the buffer
        return functionName;
    }
    strncpy(concatBuffer, modelName, FILENAME_MAX - 1);
    strncat(concatBuffer, "_", FILENAME_MAX - strlen(concatBuffer) - 1);
    strncat(concatBuffer, functionName, FILENAME_MAX - strlen(concatBuffer) - 1);
    return concatBuffer;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseStringAttributeEzXmlAndRememberPointer(
    ezxml_t element, const char* attributeName, const char** target, fmuHandle* fmu)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = duplicateAndRememberString(fmu, ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

bool
parseStringAttributeEzXml(ezxml_t element, const char* attributeName, const char** target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = _strdup(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseBooleanAttributeEzXml(ezxml_t element, const char* attributeName, bool* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = !strcmp(ezxml_attr(element, attributeName), "true");
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseFloat64AttributeEzXml(ezxml_t element, const char* attributeName, double* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = atof(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseInt16AttributeEzXml(ezxml_t element, const char* attributeName, short* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = (short)atoi(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseInt64AttributeEzXml(ezxml_t element, const char* attributeName, int64_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = atol(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseInt32AttributeEzXml(ezxml_t element, const char* attributeName, int32_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = atoi(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseFloat32AttributeEzXml(ezxml_t element, const char* attributeName, float* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = (float)atof(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseInt8AttributeEzXml(ezxml_t element, const char* attributeName, int8_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = (int8_t)atoi(ezxml_attr(element, attributeName));
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseUInt64AttributeEzXml(ezxml_t element, const char* attributeName, uint64_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = strtoul(ezxml_attr(element, attributeName), NULL, 10);
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseUInt32AttributeEzXml(ezxml_t element, const char* attributeName, uint32_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = (uint32_t)strtoul(ezxml_attr(element, attributeName), NULL, 10);
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseUInt16AttributeEzXml(ezxml_t element, const char* attributeName, uint16_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = (uint16_t)strtoul(ezxml_attr(element, attributeName), NULL, 10);
        return true;
    }
    return false;
}

//! @brief Parses specified XML attribute and assigns it to target
//! @param element XML element
//! @param attributeName Attribute name
//! @param target Pointer to target variable
//! @returns True if attribute was found, else false
bool
parseUInt8AttributeEzXml(ezxml_t element, const char* attributeName, uint8_t* target)
{
    if (ezxml_attr(element, attributeName)) {
        (*target) = (uint8_t)strtoul(ezxml_attr(element, attributeName), NULL, 10);
        return true;
    }
    return false;
}

bool
parseModelStructureElementFmi2(fmuHandle* fmu, fmi2ModelStructureHandle* output, ezxml_t* element)
{
    parseInt32AttributeEzXml(*element, "index", &output->index);

    // Default values
    output->dependencyKindsDefined = false;
    output->dependencies = NULL;
    output->dependencyKinds = NULL;

    // Count number of dependencies
    output->numberOfDependencies = 0;
    const char* dependencies = NULL;
    if (parseStringAttributeEzXml(*element, "dependencies", &dependencies)) {

        if (dependencies != NULL && dependencies[0] != '\0') {

            // Duplicate the dependencies string to make it mutable
            char* nonConstDependencies = _strdup(dependencies);
            free((char*)dependencies);

            if (nonConstDependencies == NULL) {
                return false; // strdup failed, handle as an error
            }

            // Count the number of dependencies based on space-delimited tokens
            output->numberOfDependencies = 1;
            for (int i = 0; nonConstDependencies[i]; ++i) {
                if (nonConstDependencies[i] == ' ') {
                    ++output->numberOfDependencies;
                }
            }

            // Allocate memory for dependencies
            output->dependencies
                = mallocAndRememberPointer(fmu, output->numberOfDependencies * sizeof(int));

            // Read dependencies
            const char* delim = " ";
            for (int j = 0; j < output->numberOfDependencies; ++j) {
                if (j == 0) {
                    output->dependencies[j] = atoi(strtok(nonConstDependencies, delim));
                } else {
                    output->dependencies[j] = atoi(strtok(NULL, delim));
                }
            }

            // Parse depenendency kinds element if present
            const char* dependencyKinds = NULL;
            parseStringAttributeEzXml(*element, "dependenciesKind", &dependencyKinds);
            if (dependencyKinds) {
                output->dependencyKindsDefined = true;
                char* nonConstDependencyKinds = _strdup(dependencyKinds);
                free((char*)dependencyKinds);

                // Allocate memory for dependencies (assume same number as dependencies, according
                // to FMI3 specification)
                output->dependencyKinds = mallocAndRememberPointer(
                    fmu, output->numberOfDependencies * sizeof(fmi2DependencyKind));

                // Read dependency kinds
                for (int j = 0; j < output->numberOfDependencies; ++j) {
                    const char* kind;
                    if (j == 0) {
                        kind = strtok(nonConstDependencyKinds, delim);
                    } else {
                        kind = strtok(NULL, delim);
                    }

                    if (!strcmp(kind, "dependent")) {
                        output->dependencyKinds[j] = fmi2Dependent;
                    } else if (!strcmp(kind, "constant")) {
                        output->dependencyKinds[j] = fmi2Constant;
                    } else if (!strcmp(kind, "fixed")) {
                        output->dependencyKinds[j] = fmi2Fixed;
                    } else if (!strcmp(kind, "tunable")) {
                        output->dependencyKinds[j] = fmi2Tunable;
                    } else if (!strcmp(kind, "discrete")) {
                        output->dependencyKinds[j] = fmi2Discrete;
                    } else {
                        fmi4c_printMessage("Unknown dependency kind for output dependency.");
                        free(nonConstDependencyKinds);
                        free(nonConstDependencies);
                        return false;
                    }
                }
                free(nonConstDependencyKinds);
            }
            free(nonConstDependencies);
        } else if (dependencies != NULL) {
            free((void*)dependencies); // Free if dependencies was an empty string
        }
    }

    return true;
}

bool
parseModelStructureElementFmi3(fmuHandle* fmu, fmi3ModelStructureHandle* output, ezxml_t* element)
{
    parseUInt32AttributeEzXml(*element, "valueReference", &output->valueReference);

    output->dependencyKindsDefined = false; // Default value

    // Count number of dependencies
    output->numberOfDependencies = 0;
    const char* dependencies = NULL;
    if (parseStringAttributeEzXmlAndRememberPointer(*element, "dependencies", &dependencies, fmu)) {

        if (dependencies != NULL && dependencies[0] != '\0') {

            // Duplicate the dependencies string to make it mutable
            char* nonConstDependencies = duplicateAndRememberString(fmu, dependencies);

            if (nonConstDependencies == NULL) {
                return false; // strdup failed, handle as an error
            }

            // Count the number of dependencies based on space-delimited tokens
            output->numberOfDependencies = 1;
            for (int i = 0; nonConstDependencies[i]; ++i) {
                if (nonConstDependencies[i] == ' ') {
                    ++output->numberOfDependencies;
                }
            }

            // Allocate memory for dependencies
            output->dependencies = mallocAndRememberPointer(
                fmu, output->numberOfDependencies * sizeof(fmi3ValueReference));

            // Read dependencies
            const char* delim = " ";
            for (int j = 0; j < output->numberOfDependencies; ++j) {
                if (j == 0) {
                    output->dependencies[j] = atoi(strtok(nonConstDependencies, delim));
                } else {
                    output->dependencies[j] = atoi(strtok(NULL, delim));
                }
            }

            // Parse depenendency kinds element if present
            const char* dependencyKinds = NULL;
            parseStringAttributeEzXmlAndRememberPointer(
                *element, "dependenciesKind", &dependencyKinds, fmu);
            if (dependencyKinds) {
                output->dependencyKindsDefined = true;
                char* nonConstDependencyKinds = duplicateAndRememberString(fmu, dependencyKinds);

                // Allocate memory for dependencies (assume same number as dependencies, according
                // to FMI3 specification)
                output->dependencyKinds = mallocAndRememberPointer(
                    fmu, output->numberOfDependencies * sizeof(fmi3ValueReference));

                // Read dependency kinds
                for (int j = 0; j < output->numberOfDependencies; ++j) {
                    const char* kind;
                    if (j == 0) {
                        kind = strtok(nonConstDependencyKinds, delim);
                    } else {
                        kind = strtok(NULL, delim);
                    }

                    if (!strcmp(kind, "independent")) {
                        fmi4c_printMessage("Dependency kind = \"independent\" is not allowed for "
                                           "output dependencies.");
                        return false;
                    } else if (!strcmp(kind, "constant")) {
                        output->dependencyKinds[j] = fmi3Constant;
                    } else if (!strcmp(kind, "fixed")) {
                        output->dependencyKinds[j] = fmi3Fixed;
                    } else if (!strcmp(kind, "tunable")) {
                        output->dependencyKinds[j] = fmi3Tunable;
                    } else if (!strcmp(kind, "discrete")) {
                        output->dependencyKinds[j] = fmi3Discrete;
                    } else if (!strcmp(kind, "dependent")) {
                        output->dependencyKinds[j] = fmi3Dependent;
                    } else {
                        fmi4c_printMessage("Unknown dependency kind for output dependency.");
                        return false;
                    }
                }
            }
        }
    }

    return true;
}

//! @brief Remove a directory (including all files and sub directories)
//! @param rootDirPath The path to the directory to remove
//! @param expectedDirNamePrefix Optional directory name prefix to avoid removing unintended root
//! dir. Set to Null to ignore.
//! @returns 0 if removed OK else a system error code or -1
int
removeDirectoryRecursively(const char* rootDirPath, const char* expectedDirNamePrefix)
{
    // If expectedDirNamePrefix is set, ensure that the name of the directory being removed starts
    // with this prefix This is just an optional sanity check to prevent unexpected removal of the
    // wrong directory
    if (expectedDirNamePrefix != NULL) {
        const char* lastDirName = strrchr(rootDirPath, FILE_PATH_SEPARATOR);
        // Advance to first character after separator (or point to first character if no separator
        // was found)
        lastDirName = (lastDirName) ? lastDirName + 1 : rootDirPath;
        if (strncmp(expectedDirNamePrefix, lastDirName, strlen(expectedDirNamePrefix)) != 0) {
            printf("Directory name prefix '%s' mismatch, refusing to remove directory '%s'\n",
                expectedDirNamePrefix, rootDirPath);
            return 1;
        }
    }

    int status = 0;

#ifdef _MSC_VER
    WIN32_FIND_DATA findFileData;
    char searchPath[MAX_PATH];
    snprintf(searchPath, sizeof(searchPath), "%s\\*", rootDirPath);

    HANDLE hFind = FindFirstFile(searchPath, &findFileData);
    if (hFind == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "Could not open directory: %s\n", rootDirPath);
        return -1;
    }

    do {
        const char* name = findFileData.cFileName;
        // Avoid recursing upwards in the directory structure
        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) {
            continue;
        }

        char fullPath[MAX_PATH];
        snprintf(fullPath, sizeof(fullPath), "%s\\%s", rootDirPath, name);

        if (findFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            // Recursively delete subdirectory
            if (removeDirectoryRecursively(fullPath, NULL) != 0) {
                status = -1;
                break;
            }
        } else {
            // Delete file
            if (DeleteFile(fullPath) == 0) {
                fprintf(stderr, "Could not delete file: %s\n", fullPath);
                status = -1;
                break;
            }
        }
    } while (FindNextFile(hFind, &findFileData) != 0);

    FindClose(hFind);

    // Remove the directory itself
    if (status == 0 && _rmdir(rootDirPath) != 0) {
        fprintf(stderr, "Could not remove directory: %s\n", rootDirPath);
        status = -1;
    }

#else // POSIX
    DIR* dir = opendir(rootDirPath);
    if (!dir) {
        perror("Could not open directory");
        fprintf(stderr, "Could not access '%s' for removal\n", rootDirPath);
        return -1;
    }

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        // Avoid recursing upwards in the directory structure
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        // Determine full path to file or directory for current entry
        char fullPath[4096];
        snprintf(fullPath, sizeof(fullPath), "%s/%s", rootDirPath, entry->d_name);

        // Figure out if entry represents a directory or not
        bool entryIsDir = false;
        struct stat statbuf;
#ifdef _DIRENT_HAVE_D_TYPE
        entryIsDir = (entry->d_type == DT_DIR);
        // On some filesystems d_type is not set, then use stat to check the type
        if (entry->d_type == DT_UNKNOWN) {
            if (stat(fullPath, &statbuf) == 0) {
                entryIsDir = S_ISDIR(statbuf.st_mode);
            }
        }
#else
        // On some systems d_type is note present, in which case stats is used
        if (stat(fullPath, &statbuf) == 0) {
            entryIsDir = S_ISDIR(statbuf.st_mode);
        }
#endif

        if (entryIsDir) {
            // Recursive delete for subdirectory
            if (removeDirectoryRecursively(fullPath, NULL) != 0) {
                status = -1;
                break;
            }
        } else {
            // Delete file
            if (unlink(fullPath) != 0) {
                perror("Could not delete file");
                fprintf(stderr, "Could not delete '%s'\n", fullPath);
                status = -1;
                break;
            }
        }
    }

    closedir(dir);

    // Remove the directory itself
    if (status == 0 && rmdir(rootDirPath) != 0) {
        perror("Could not remove directory");
        fprintf(stderr, "Could not remove '%s'\n", rootDirPath);
        status = -1;
    }
#endif

    return status;
}
