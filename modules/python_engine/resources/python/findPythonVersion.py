# =============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# =============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# =============================================================================
import sys
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
    customizeOutput()  # Call the function to customize the output.
    
    # Get the Python version and format it as a string.
    version = '%d.%d' % (sys.version_info[0], sys.version_info[1])
    
    # Print the Python version.
    print(version)