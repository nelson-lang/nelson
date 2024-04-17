%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
install_numpy = ["import pip"; "pip.main(['install', 'numpy'])"]
output = evalc('pyrun(install_numpy)');
%=============================================================================
output = evalc('pyrun(install_numpy)');
assert_istrue(length(output) > 1000);