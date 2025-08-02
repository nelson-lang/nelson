%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CHECK REF-->
%=============================================================================
% Nelson Interface Generator (NIG) example

NIG_FUNCTION = struct();
NIG_FUNCTION.NELSON_NAME = 'example_nig_sum';
NIG_FUNCTION.NELSON_NAMESPACE = 'Example'; % optional
NIG_FUNCTION.MODULE_NAME = 'example';
NIG_FUNCTION.SYMBOL = 'sum';
NIG_FUNCTION.LANGUAGE = 'fortran';

NIG_FUNCTION.VARIABLES = struct([]);
IDX = length(NIG_FUNCTION.VARIABLES) + 1;
NIG_FUNCTION.VARIABLES(IDX).NAME = 'A';
NIG_FUNCTION.VARIABLES(IDX).TYPE = 'integer';
NIG_FUNCTION.VARIABLES(IDX).MODE = 'input';

IDX = length(NIG_FUNCTION.VARIABLES) + 1;
NIG_FUNCTION.VARIABLES(IDX).NAME = 'B';
NIG_FUNCTION.VARIABLES(IDX).TYPE = 'integer';
NIG_FUNCTION.VARIABLES(IDX).MODE = 'input';

IDX = length(NIG_FUNCTION.VARIABLES) + 1;
NIG_FUNCTION.VARIABLES(IDX).NAME = 'OUTPUT';
NIG_FUNCTION.VARIABLES(IDX).TYPE = 'integer';
NIG_FUNCTION.VARIABLES(IDX).MODE = 'output';
%=============================================================================
dir_dest = [tempdir(), 'test_nig'];
if isdir(dir_dest)
  rmdir(dir_dest, 's');
end
mkdir(dir_dest);
%=============================================================================
nig(NIG_FUNCTION, dir_dest);
fileread([dir_dest,'/Gateway.cpp'])
fileread([dir_dest,'/example_nig_sumBuiltin.hpp'])
fileread([dir_dest,'/example_nig_sumBuiltin.cpp'])
%=============================================================================
rmdir(dir_dest, 's');
%=============================================================================
