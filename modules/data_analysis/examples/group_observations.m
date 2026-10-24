%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Count observations by category and display a summary table.
observations = table({'north'; 'south'; 'north'; 'east'; 'south'; 'north'}, ...
  [12; 8; 15; 11; 10; 16], 'VariableNames', {'Region', 'Value'});
summary = groupcounts(observations, 'Region');
disp(summary);
