%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ParquetDatastore < handle
  properties
    Files = {};
    ReadSize = 'file';
    SelectedVariableNames = {};
    OutputType = 'table';
    RowTimes = [];
    RowFilter = [];
    VariableNamingRule = 'modify';
  end
  properties (Access = private)
    CurrentFileIndex = 1;
  end
  properties (Dependent)
    VariableNames
  end
  methods
    function obj = ParquetDatastore(location, varargin)
      obj.Files = resolveParquetFiles(location);
      obj = parseDatastoreOptions(obj, varargin{:});
    end
    function names = get.VariableNames(obj)
      if isempty(obj.Files)
        names = {};
      else
        info = parquetinfo(obj.Files{1});
        names = info.VariableNames;
      end
    end
    function tf = hasdata(obj)
      tf = obj.CurrentFileIndex <= length(obj.Files);
    end
    function obj = reset(obj)
      obj.CurrentFileIndex = 1;
    end
    function [T, info] = read(obj)
      if ~hasdata(obj)
        error('Nelson:ParquetDatastore:noMoreData', 'No more data to read.');
      end
      filename = obj.Files{obj.CurrentFileIndex};
      T = parquetread(filename, 'SelectedVariableNames', obj.SelectedVariableNames, ...
        'OutputType', obj.OutputType, 'RowTimes', obj.RowTimes, ...
        'RowFilter', obj.RowFilter, 'VariableNamingRule', obj.VariableNamingRule);
      info = struct('Filename', filename, 'FileIndex', obj.CurrentFileIndex);
      obj.CurrentFileIndex = obj.CurrentFileIndex + 1;
    end
    function T = readall(obj)
      obj = reset(obj);
      chunks = {};
      while hasdata(obj)
        [chunk, obj] = readChunk(obj);
        chunks{end + 1} = chunk;
      end
      if isempty(chunks)
        T = table();
      else
        T = vertcat(chunks{:});
      end
    end
    function T = preview(obj)
      if isempty(obj.Files)
        T = table();
      else
        T = head(parquetread(obj.Files{1}, 'SelectedVariableNames', obj.SelectedVariableNames));
      end
    end
  end
end
%=============================================================================
function [T, obj] = readChunk(obj)
  [T, ~] = read(obj);
end
%=============================================================================
function obj = parseDatastoreOptions(obj, varargin)
  if mod(nargin - 1, 2) ~= 0
    error('Nelson:parquet:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(varargin)
    name = lower(char(varargin{k}));
    value = varargin{k + 1};
    switch name
      case 'readsize'
        obj.ReadSize = value;
      case 'selectedvariablenames'
        obj.SelectedVariableNames = datastoreStringList(value);
      case 'outputtype'
        obj.OutputType = char(value);
      case 'rowtimes'
        obj.RowTimes = value;
      case 'rowfilter'
        obj.RowFilter = value;
      case 'variablenamingrule'
        obj.VariableNamingRule = char(value);
      otherwise
        error('Nelson:ParquetDatastore:unsupportedOption', 'Unsupported parquetDatastore option.');
    end
  end
end
%=============================================================================
function names = datastoreStringList(value)
  if isempty(value)
    names = {};
  elseif ischar(value)
    names = {value};
  elseif isstring(value)
    names = cellstr(value);
  else
    names = value;
  end
end
%=============================================================================
function files = resolveParquetFiles(location)
  if ischar(location) || isstring(location)
    locations = cellstr(location);
  else
    locations = location;
  end
  files = {};
  for k = 1:length(locations)
    item = char(locations{k});
    if isdir(item)
      listing = dir([item, filesep(), '*.parquet']);
      for j = 1:length(listing)
        files{end + 1} = [item, filesep(), listing(j).name];
      end
    else
      listing = dir(item);
      for j = 1:length(listing)
        files{end + 1} = [listing(j).folder, filesep(), listing(j).name];
      end
    end
  end
end
%=============================================================================
