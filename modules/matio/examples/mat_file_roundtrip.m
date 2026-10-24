%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Save selected variables in a temporary MAT file and reload them.
filename = [tempdir(), 'nelson-example.mat'];
labels = {'north', 'south'};
values = [12.3, 15.8];
save(filename, 'labels', 'values');
clear labels values;
load(filename);
disp(table(labels', values', 'VariableNames', {'Region', 'Value'}));
