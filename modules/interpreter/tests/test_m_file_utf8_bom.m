%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
script_file = [path_test, '/script_with_utf8_bom.m'];
fid = fopen(script_file, 'wb');
fwrite(fid, uint8([239 187 191]), 'uint8');
fwrite(fid, uint8('bomValue = 42;'), 'uint8');
fclose(fid);
clear bomValue;
run(script_file);
asserts.isequal(bomValue, 42);
rmdir(path_test, 's');
%=============================================================================
