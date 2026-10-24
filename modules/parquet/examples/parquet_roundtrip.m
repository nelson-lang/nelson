%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Write a typed table to Parquet and read it back.
filename = [tempdir(), 'nelson-example.parquet'];
measurements = table([1; 2; 3], {'A'; 'B'; 'A'}, [4.2; 5.1; 4.8], ...
  'VariableNames', {'Id', 'Group', 'Value'});
parquetwrite(filename, measurements);
restored = parquetread(filename);
disp(restored);
