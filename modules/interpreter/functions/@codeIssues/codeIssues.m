%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef codeIssues
  properties (SetAccess = private)
    Date
    Release
    Files
    CodeAnalyzerConfiguration
    Issues
    SuppressedIssues
  end

  properties (Hidden, SetAccess = private)
    RawDiagnostics
    IncludeSubfolders
  end

  methods
    function obj = codeIssues(varargin)
      names = {pwd()};
      includeSubfolders = false;
      configFile = '';

      if nargin > 0
        first = varargin{1};
        if isNamesArgument(first)
          names = normalizeNames(first);
          varargin(1) = [];
        end
      end
      if mod(length(varargin), 2) ~= 0
        error('Nelson:codeIssues:invalidArgument', 'Name-value arguments expected.');
      end
      for k = 1:2:length(varargin)
        name = normalizeName(varargin{k});
        value = varargin{k + 1};
        switch lower(name)
          case 'codeanalyzerconfiguration'
            configFile = normalizeName(value);
          case 'includesubfolders'
            includeSubfolders = logical(value);
          otherwise
            error('Nelson:codeIssues:unsupportedArgument', 'Unsupported name-value argument.');
        end
      end

      diagnostics = __code_analyzer__(names, includeSubfolders, configFile, true, true, true, true);
      suppressed = [diagnostics.Suppressed];
      obj.Date = datestr(now());
      obj.Release = version();
      obj.Files = names;
      obj.CodeAnalyzerConfiguration = configFile;
      obj.IncludeSubfolders = includeSubfolders;
      obj.RawDiagnostics = diagnostics;
      obj.Issues = diagnosticsToTable(diagnostics(~suppressed));
      obj.SuppressedIssues = diagnosticsToTable(diagnostics(suppressed));
    end
  end
end
%=============================================================================
function tf = isNamesArgument(value)
  tf = ischar(value) || iscell(value) || isstring(value);
end
%=============================================================================
function names = normalizeNames(value)
  if ischar(value)
    names = {value};
  elseif isstring(value)
    names = cellstr(value);
  elseif iscell(value)
    names = value;
  else
    error('Nelson:types:fileNamesNotText', 'File names must be text.');
  end
end
%=============================================================================
function name = normalizeName(value)
  if isstring(value) && isscalar(value)
    name = char(value);
  elseif ischar(value)
    name = value;
  else
    error('Nelson:types:nameNotText', 'Name must be text.');
  end
end
%=============================================================================
function T = diagnosticsToTable(diagnostics)
  if isempty(diagnostics)
    T = table(cell(0, 1), cell(0, 1), cell(0, 1), cell(0, 1), cell(0, 1), ...
      zeros(0, 1), zeros(0, 1), zeros(0, 1), zeros(0, 1), cell(0, 1), ...
      cell(0, 1), cell(0, 1), cell(0, 1), cell(0, 1), ...
      'VariableNames', {'Location', 'Severity', 'Fixability', 'Description', 'CheckID', ...
      'LineStart', 'LineEnd', 'ColumnStart', 'ColumnEnd', 'FullFilename', 'Category', ...
      'RuleName', 'HelpURI', 'Fingerprint'});
    return
  end
  Location = {diagnostics.Location}';
  Severity = {diagnostics.Severity}';
  Fixability = {diagnostics.Fixability}';
  Description = {diagnostics.Description}';
  CheckID = {diagnostics.CheckID}';
  LineStart = [diagnostics.LineStart]';
  LineEnd = [diagnostics.LineEnd]';
  ColumnStart = [diagnostics.ColumnStart]';
  ColumnEnd = [diagnostics.ColumnEnd]';
  FullFilename = {diagnostics.FullFilename}';
  Category = {diagnostics.Category}';
  RuleName = {diagnostics.RuleName}';
  HelpURI = {diagnostics.HelpURI}';
  Fingerprint = {diagnostics.Fingerprint}';
  T = table(Location, Severity, Fixability, Description, CheckID, LineStart, LineEnd, ...
    ColumnStart, ColumnEnd, FullFilename, Category, RuleName, HelpURI, Fingerprint, ...
    'VariableNames', {'Location', 'Severity', 'Fixability', 'Description', 'CheckID', ...
    'LineStart', 'LineEnd', 'ColumnStart', 'ColumnEnd', 'FullFilename', 'Category', ...
    'RuleName', 'HelpURI', 'Fingerprint'});
end
%=============================================================================
