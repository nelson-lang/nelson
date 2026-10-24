%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function parquetwrite(filename, T, varargin)
  [opts, T] = parquetParseWriteOptions(T, varargin{:});
  _parquetwrite(filename, T, opts);
end
%=============================================================================
function [opts, T] = parquetParseWriteOptions(T, varargin)
  opts.Compression = 'snappy';
  opts.Encoding = 'auto';
  opts.VariableNames = {};
  opts.RowGroupHeights = [];
  opts.Version = '2.6';
  if mod(length(varargin), 2) ~= 0
    error('Nelson:parquet:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(varargin)
    name = char(varargin{k});
    value = varargin{k + 1};
    switch lower(name)
      case 'variablecompression'
        opts.Compression = parquetScalarText(value);
      case 'variableencoding'
        opts.Encoding = parquetScalarText(value);
      case 'variablenames'
        opts.VariableNames = value;
      case 'rowgroupheights'
        opts.RowGroupHeights = value;
      case 'version'
        opts.Version = parquetScalarText(value);
      otherwise
        error('Nelson:parquetwrite:unsupportedOption', 'Unsupported parquetwrite option.');
    end
  end
  if ~isempty(opts.VariableNames)
    T.Properties.VariableNames = cellstr(opts.VariableNames);
  end
end
%=============================================================================
function value = parquetScalarText(value)
  if isstring(value)
    value = char(value);
  elseif iscell(value)
    value = char(value{1});
  else
    value = char(value);
  end
  value = lower(value);
end
%=============================================================================
