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
folder = tempname();
mkdir(folder);
cleanupFolder = onCleanup(@() rmdir(folder, 's')); %#ok<NASGU>
% Always drop the path again, even if an assertion fails, so the shared
% BlockRegistry is left as the test found it.
cleanupPath = onCleanup(@() NFlow.rmlibrarypath(folder)); %#ok<NASGU>
%=============================================================================
baseline = NFlow.librarypaths();
asserts.istrue(iscell(baseline));
%=============================================================================
% add returns the updated list and the folder is present.
paths = NFlow.addlibrarypath(folder);
asserts.istrue(iscell(paths));
asserts.istrue(any(strcmp(paths, folder)));
asserts.isequal(numel(paths), numel(baseline) + 1);
%=============================================================================
% librarypaths reports the same list.
asserts.istrue(any(strcmp(NFlow.librarypaths(), folder)));
%=============================================================================
% adding the same folder again is a no-op.
paths = NFlow.addlibrarypath(folder);
asserts.isequal(numel(paths), numel(baseline) + 1);
%=============================================================================
% remove takes it back out.
paths = NFlow.rmlibrarypath(folder);
asserts.isfalse(any(strcmp(paths, folder)));
asserts.isequal(numel(paths), numel(baseline));
%=============================================================================
% a non-existent directory is rejected.
asserts.checkerror('NFlow.addlibrarypath([tempname(), ''_nope''])', ...
  _('A valid existing directory is expected.'));
%=============================================================================
