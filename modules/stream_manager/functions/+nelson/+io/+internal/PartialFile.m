%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef PartialFile < handle
  % Shared part of matfile and nh5file: obj.name(i1, ..., iN) reads and writes parts of variables.
  properties (SetAccess = private)
    Properties
  end

  methods (Abstract, Access = protected)
    value = readVariable(obj, name, subscripts)
    writeVariable(obj, name, value, subscripts)
    dims = variableSize(obj, name)
    varargout = fileWho(obj, varargin)
    varargout = fileWhos(obj, varargin)
  end

  methods (Hidden)
    function obj = PartialFile(source, extension, options)
      obj.Properties = nelson.io.internal.FileProperties(resolveSource(source, extension));
      applyOptions(obj.Properties, options);
    end

    function varargout = subsref(obj, s)
      name = accessedName(s);
      if strcmp(name, 'Properties')
        varargout{1} = readDescription(obj.Properties, s);
        return
      end
      if isscalar(s)
        varargout{1} = readVariable(obj, name, []);
      else
        varargout{1} = readVariable(obj, name, parenSubscripts(s, name));
      end
    end

    function obj = subsasgn(obj, s, value)
      name = accessedName(s);
      if strcmp(name, 'Properties')
        writeDescription(obj.Properties, s, value);
        return
      end
      if ~obj.Properties.Writable
        error('Nelson:io:readOnly', '''%s'' cannot be modified: Properties.Writable is false.', name);
      end
      if isscalar(s)
        writeVariable(obj, name, value, []);
      else
        writeVariable(obj, name, value, parenSubscripts(s, name));
      end
    end

    function varargout = size(obj, varargin)
      narginchk(1, 3);
      if nargin == 1 || (nargin == 2 && ~isText(varargin{1}))
        [varargout{1:max(nargout, 1)}] = size(1, varargin{:});
        return
      end
      if nargin == 3
        validateattributes(varargin{2}, {'numeric'}, {'scalar', 'positive', 'integer'});
      end
      dims = variableSize(obj, char(varargin{1}));
      if nargin == 3
        dims(end + 1:varargin{2}) = 1;
        varargout{1} = dims(varargin{2});
      elseif nargout <= 1
        varargout{1} = dims;
      else
        varargout = splitSize(dims, nargout);
      end
    end

    function varargout = who(obj, varargin)
      nargoutchk(0, 1);
      names = fileVariables(obj, varargin);
      if nargout == 1
        varargout{1} = names;
      elseif ~isempty(names)
        fileWho(obj, names{:});
      end
    end

    function varargout = whos(obj, varargin)
      nargoutchk(0, 1);
      names = fileVariables(obj, varargin);
      if isempty(names)
        if nargout == 1
          varargout{1} = reshape(struct('name', {}, 'size', {}, 'bytes', {}, 'class', {}, ...
            'global', {}, 'sparse', {}, 'complex', {}, 'nesting', {}, 'persistent', {}), 0, 1);
        end
      elseif nargout == 1
        varargout{1} = fileWhos(obj, names{:});
      else
        fileWhos(obj, names{:});
      end
    end

    function disp(obj)
      source = obj.Properties.Source;
      labels = {'Properties.Source'; 'Properties.Writable'};
      values = {['''', source, '''']; mat2str(obj.Properties.Writable)};
      if isfile(source)
        info = fileWhos(obj);
        labels = [labels; {info.name}'];
        values = [values; arrayfun(@(v) ['[', sizeText(v.size), ' ', v.class, ']'], info, ...
          'UniformOutput', false)];
      end
      fprintf('  %s\n\n  Properties:\n', class(obj));
      width = max(cellfun(@numel, labels));
      for k = 1:numel(labels)
        fprintf('    %s%s: %s\n', blanks(width - numel(labels{k})), labels{k}, values{k});
      end
      fprintf('\n');
    end

    function display(obj, varargin)
      if nargin > 1
        name = varargin{1};
      else
        name = inputname(1);
      end
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('%s = \n\n', name);
      else
        fprintf('%s = \n', name);
      end
      disp(obj);
    end

    function varargout = horzcat(varargin) %#ok<STOUT>
      errorConcatenate(varargin{1});
    end

    function varargout = vertcat(varargin) %#ok<STOUT>
      errorConcatenate(varargin{1});
    end

    function varargout = cat(varargin) %#ok<STOUT>
      errorConcatenate(varargin{2});
    end
  end

  methods (Access = private)
    function names = fileVariables(obj, patterns)
      % sorted names of the file variables matching the who/whos patterns ('*' wildcard or -regexp)
      if ~isfile(obj.Properties.Source)
        names = {};
        return
      end
      names = fileWho(obj);
      names = names(:);
      if ~isempty(patterns)
        names = names(matchPatterns(names, patterns));
      end
    end
  end
end
%=============================================================================
function source = resolveSource(source, extension)
  if isStringScalar(source)
    source = char(source);
  end
  if ~ischar(source) || ~isrow(source)
    error('Nelson:io:fileNameExpected', 'File name: string scalar or character vector expected.');
  end
  [~, ~, ext] = fileparts(source);
  if isempty(ext) && ~isfile(source)
    source = [source, extension];
  end
  if ~isAbsolutePath(source)
    source = fullPathOf(source);
  end
end
%=============================================================================
function tf = isAbsolutePath(source)
  tf = startsWith(source, {'/', '\'}) || (numel(source) > 1 && source(2) == ':');
end
%=============================================================================
function source = fullPathOf(source)
  % the current folder first, then the search path
  if ~isfile(source)
    found = which(source);
    if ~isempty(found) && isfile(found)
      source = found;
      return
    end
  end
  source = fullfile(pwd(), source);
end
%=============================================================================
function applyOptions(description, options)
  if mod(numel(options), 2) ~= 0
    error('Nelson:data_structures:optionsNameValuePairs', 'Options must be name-value pairs.');
  end
  for k = 1:2:numel(options)
    if ~isText(options{k}) || ~strcmpi(options{k}, 'Writable')
      error('Nelson:io:unknownOption', 'Unknown option ''%s'': only ''Writable'' is supported.', ...
        char(string(options{k})));
    end
    description.Writable = options{k + 1};
  end
end
%=============================================================================
function tf = isText(value)
  tf = (ischar(value) && isrow(value)) || isStringScalar(value);
end
%=============================================================================
function name = accessedName(s)
  if ~strcmp(s(1).type, '.')
    error('Nelson:io:dotAccessOnly', 'Use m.name or m.name(i1, ..., iN) to access a variable.');
  end
  name = s(1).subs;
  if isStringScalar(name)
    name = char(name);
  elseif ~ischar(name)
    error('Nelson:io:invalidVariableName', 'Variable name must be text.');
  end
end
%=============================================================================
function subscripts = parenSubscripts(s, name)
  if numel(s) > 2 || ~strcmp(s(2).type, '()')
    error('Nelson:io:parenIndexOnly', '''%s'' accepts a single level of () indexing.', name);
  end
  subscripts = s(2).subs;
end
%=============================================================================
function value = readDescription(description, s)
  value = description;
  for k = 2:numel(s)
    checkDescriptionIndexing(s(k));
    value = value.(s(k).subs);
  end
end
%=============================================================================
function writeDescription(description, s, value)
  if isscalar(s)
    % obj.Properties.name = v changes the description in place, then stores it back
    if ~(isa(value, 'nelson.io.internal.FileProperties') && value == description)
      error('Nelson:io:reservedName', '''Properties'' is reserved and cannot be used as a variable name.');
    end
    return
  end
  checkDescriptionIndexing(s(2));
  description.(s(2).subs) = value;
end
%=============================================================================
function checkDescriptionIndexing(level)
  if ~strcmp(level.type, '.')
    error('Nelson:io:propertiesFieldAccess', 'Use m.Properties.Source or m.Properties.Writable.');
  end
end
%=============================================================================
function errorConcatenate(obj)
  error('Nelson:io:noConcatenation', '%s objects cannot be concatenated.', class(obj));
end
%=============================================================================
function out = splitSize(dims, n)
  % [d1, ..., dn] = size(obj, name): missing trailing sizes are 1, the last one folds the rest
  out = num2cell(ones(1, n));
  m = numel(dims);
  if n > m
    out(1:m) = num2cell(dims);
  else
    out(1:n - 1) = num2cell(dims(1:n - 1));
    out{n} = prod(dims(n:m));
  end
end
%=============================================================================
function keep = matchPatterns(names, patterns)
  keep = false(size(names));
  useRegexp = false;
  for k = 1:numel(patterns)
    pattern = char(patterns{k});
    if strcmp(pattern, '-regexp')
      useRegexp = true;
      continue
    end
    if ~useRegexp
      pattern = ['^', regexptranslate('wildcard', pattern), '$'];
    end
    keep = keep | ~cellfun(@isempty, regexp(names, pattern, 'once'));
  end
end
%=============================================================================
function text = sizeText(sz)
  switch numel(sz)
    case 0
      text = ' - ';
    case {1, 2}
      text = sprintf('%dx%d', sz(1), sz(end));
    otherwise
      text = sprintf('%d-D', numel(sz));
  end
end
%=============================================================================
