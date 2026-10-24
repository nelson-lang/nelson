%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A missing JSON key and a missing JSON Pointer each carry their own identifier.
%=============================================================================
filename = [tempname(), '.json'];
fid = fopen(filename, 'w', 'n', 'UTF-8');
fprintf(fid, '%s', '{"a": {"rows": [{"id": 1}, {"id": 2}]}}');
fclose(fid);
asserts.checkerror('readtable(filename, ''TableNodeName'', ''nothing'')', 'JSON key not found: nothing', 'Nelson:spreadsheet:jsonKeyNotFound');
asserts.checkerror('readtable(filename, ''TableSelector'', ''/a/nothing'')', 'JSON Pointer not found: /a/nothing', 'Nelson:spreadsheet:jsonPointerNotFound');
delete(filename);
%=============================================================================
