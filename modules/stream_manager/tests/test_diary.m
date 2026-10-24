%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
filename = fullfile(folder, 'diary_direct.txt');
%=============================================================================
diary('off');
diary('set', 'DiaryFile', filename);
asserts.isequal(diary('get', 'DiaryFile'), filename);
%=============================================================================
diary('on');
asserts.isequal(diary('get', 'Diary'), 'on');
disp('nelson diary direct test');
diary('off');
asserts.isequal(diary('get', 'Diary'), 'off');
%=============================================================================
if isfile(filename)
  rmfile(filename);
end
rmdir(folder);
%=============================================================================
