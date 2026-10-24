%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('copyfile'), 3);
asserts.isequal(nargout('copyfile'), 3);
%=============================================================================
source = [tempname(), '.txt'];
destination = [tempname(), '.txt'];
filewrite(source, 'Nelson');
[status, msg] = copyfile(source, destination);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.istrue(isfile(destination));
asserts.isequal(fileread(destination), 'Nelson');
rmfile(source);
rmfile(destination);
%=============================================================================
% three output arguments: [status, msg, msgID]
source = [tempname(), '.txt'];
destination = [tempname(), '.txt'];
filewrite(source, 'Nelson');
[status, msg, msgID] = copyfile(source, destination);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isequal(msgID, '');
asserts.istrue(isfile(destination));
rmfile(source);
rmfile(destination);
%=============================================================================
% three output arguments on failure (missing source) must not throw
source = [tempname(), '.txt'];
destination = [tempname(), '.txt'];
[status, msg, msgID] = copyfile(source, destination);
asserts.isfalse(status);
asserts.istrue(~isempty(msg));
asserts.istrue(~isempty(msgID));
%=============================================================================
asserts.checkerror('copyfile(''nonexistent_source_for_id_lock'', tempdir())', _('Directory source does not exist.'));
asserts.checkerror('copyfile(''nonexistent_source_for_id_lock'', tempdir())', _('Directory source does not exist.'), 'Nelson:files_folders_functions:directorySourceDoesNotExist');
%=============================================================================
