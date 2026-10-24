%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mkdir'), 2);
asserts.isequal(nargout('mkdir'), 3);
%=============================================================================
pathTest = tempname();
[status, msg] = mkdir(pathTest);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.istrue(isdir(pathTest));
rmdir(pathTest);
%=============================================================================
parentDir = tempname();
mkdir(parentDir);
[status, msg] = mkdir(parentDir, 'child');
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.istrue(isdir(fullfile(parentDir, 'child')));
rmdir(parentDir, 's');
%=============================================================================
% three output arguments on a new directory: status = true, message and
% identifier are empty.
pathTest = tempname();
[status, msg, msgID] = mkdir(pathTest);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isequal(msgID, '');
asserts.istrue(isdir(pathTest));
%=============================================================================
% three output arguments on an already existing directory: status stays true
% (success semantics), message and identifier are not empty.
[status, msg, msgID] = mkdir(pathTest);
asserts.istrue(status);
asserts.istrue(~isempty(msg));
asserts.istrue(~isempty(msgID));
asserts.isequal(msgID, 'Nelson:mkdir:directoryExists');
rmdir(pathTest);
%=============================================================================
% three output arguments with a parent directory argument.
parentDir = tempname();
mkdir(parentDir);
[status, msg, msgID] = mkdir(parentDir, 'child');
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isequal(msgID, '');
asserts.istrue(isdir(fullfile(parentDir, 'child')));
rmdir(parentDir, 's');
%=============================================================================
