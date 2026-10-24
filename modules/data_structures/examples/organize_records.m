%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Organize heterogeneous records with structures and cell arrays.
record(1) = struct('Name', 'sensor-a', 'Samples', [2.1, 2.4, 2.2]);
record(2) = struct('Name', 'sensor-b', 'Samples', [3.0, 2.9, 3.2]);
for k = 1:length(record)
  disp([record(k).Name, ': ', mat2str(mean(record(k).Samples))]);
end
