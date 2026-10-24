%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('feof'), 1);
asserts.isequal(nargout('feof'), 1);
%=============================================================================
filename = [modulepath('characters_encoding', 'tests'), '/shisei_UTF-8.txt'];
fid = fopen(filename, 'rt', 'n', 'UTF-8');
asserts.isequal(feof(fid), 0);
fclose(fid);
%=============================================================================
