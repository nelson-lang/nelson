%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Join measurements with station metadata using a shared key.
measurements = table([101; 102; 103], [18.2; 21.5; 19.8], ...
  'VariableNames', {'Station', 'Temperature'});
metadata = table([101; 102; 104], {'North'; 'Center'; 'South'}, ...
  'VariableNames', {'Station', 'Area'});
combined = outerjoin(measurements, metadata, 'Keys', 'Station', 'MergeKeys', true);
disp(combined);
