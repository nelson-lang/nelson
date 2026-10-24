%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--REUSE PROCESS-->
%=============================================================================
group = [randi(20, 50000, 1), randi(12, 50000, 1), randi(8, 50000, 1)];
tic();
D = dummyvar(group);
t = toc();
asserts.isequal(size(D), [50000 40]);
asserts.istrue(t >= 0);
%=============================================================================
labels = cell(40000, 1);
for k = 1:length(labels)
  labels{k} = ['group_', int2str(mod(k, 50))];
end
tic();
D = dummyvar(labels);
t = toc();
asserts.isequal(size(D), [40000 50]);
asserts.istrue(t >= 0);
%=============================================================================
