# =============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# =============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# =============================================================================
import sys
import os
from ctypes.util import find_library

# =============================================================================
# Function to find the library in a specific folder.
def findFolder(folder, libName):
    # Check if the folder exists.
    if os.path.exists(folder):
        # Define a function to check if a file matches the library name.
        def isMatch(name):
            slName = (libName, 'lib' + libName)
            slExt = ('.dll', '.so', '.dylib')
            return name.startswith(slName) and name.endswith(slExt)
        # Get the list of files in the folder that match the criteria.
        names = []
        try:
            names = [n for n in os.listdir(folder) if isMatch(n)]
        except PermissionError:
            pass
        # If matching files are found, return the path of the first one.
        if names:
            return os.path.join(folder, names[0])
    return None

# =============================================================================
# Function to find the library in the executable folder.
def findLibrayWithExecutable(libName):
    # Get the path of the folder containing the executable.
    executableFolder = os.path.split(sys.executable)[0]
    # Search for the library in the executable folder.
    return findFolder(executableFolder, libName)

# =============================================================================
# Function to find the library in various system directories.
def findLibrayWithPrefix(libName):
    # Get possible locations where the library might be installed.
    # https://docs.python.org/3/library/sys.html
    possibleHomes = [getattr(sys, prefix, '') for prefix in ['prefix', 'exec_prefix', 'base_prefix', 'base_exec_prefix']]
    # Iterate over each possible location.
    for home in possibleHomes:
        if home:
            # Check if the library exists in the current location.
            abPath = findFolder(home, libName)
            if abPath:
                return abPath
            # Check if the library exists in the 'lib' subfolder.
            libFolder = os.path.join(home, 'lib')
            abPath = findFolder(libFolder, libName)
            if abPath:
                return abPath
            # Check if the library exists in the 'bin' subfolder.
            libFolder = os.path.join(home, 'bin')
            abPath = findFolder(libFolder, libName)
            if abPath:
                return abPath

# =============================================================================
# Function to find the library using ctypes.
def findLibrayWithCTypes(libName):
    # Use ctypes to find the library.
    if os.name == 'nt':
        libPath = find_library(libName) or find_library(libName + '.dll')
    else:
        libPath = find_library(libName)
    return str(libPath) if libPath else None

# =============================================================================
# Function to search for library locations.
def searchLocations(libName):
    # List to store the search locations.
    locations = []
    # Determine the appropriate library name based on OS.
    if os.name == 'nt':
        libName = f'python{sys.version_info[0]}{sys.version_info[1]}'
    else:
        libName = f'python{sys.version_info[0]}.{sys.version_info[1]}'
    # Add methods for finding the library.
    locations.append(findLibrayWithExecutable)
    locations.append(findLibrayWithPrefix)
    locations.append(findLibrayWithCTypes)
    return locations

# =============================================================================
def customizeOutput():
    # Try to reconfigure stdout encoding to UTF-8.
    try:
        sys.stdout.reconfigure(encoding='utf-8')
    except:
        # If reconfiguration fails, continue without raising an error.
        pass  # Pass without doing anything.

# =============================================================================
# Main section of the script.
if __name__ == '__main__':
    customizeOutput()
    # Determine the appropriate library name based on OS.
    libName = f'python{sys.version_info[0]}{sys.version_info[1]}' if os.name == 'nt' else f'python{sys.version_info[0]}.{sys.version_info[1]}'
    # Search for the library in different locations.
    locations = searchLocations(libName)
    # Iterate over each search function until the library is found.
    for function in locations:
        res = function(libName)
        if res is not None:
            print(res)
            break

# =============================================================================
