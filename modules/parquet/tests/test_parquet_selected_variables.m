%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
filename = [tempname(), '.parquet'];
cleanup = onCleanup(@() deleteIfExists(filename));
T = table([1; 2; 3], [10; 20; 30], [100; 200; 300], 'VariableNames', {'A', 'B', 'C'});
parquetwrite(filename, T);
R = parquetread(filename, 'SelectedVariableNames', {'C', 'A'});
asserts.isequal(R.Properties.VariableNames, {'C', 'A'});
asserts.isequal(R.C, T.C);
asserts.isequal(R.A, T.A);
%=============================================================================
function deleteIfExists(f)
  if isfile(f)
    rmfile(f);
  end
end
%=============================================================================
