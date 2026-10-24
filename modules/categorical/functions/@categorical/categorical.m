%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef categorical
  properties (Hidden)
    codes = zeros(0, 0)
    categoryNames = {}
    isProtected = false
    isOrdinal = false
  end

  methods
    function obj = categorical(inputData, varargin)
      if nargin == 0
        return
      end
      if nargin == 1 && iscell(inputData)
        [codes, categoryNames, usedFastPath] = __categorical_cellstr_codes__(inputData);
        if usedFastPath
          obj.codes = codes;
          obj.categoryNames = categoryNames;
          return
        end
      end
      if ~isempty(varargin) && isstruct(varargin{end}) && isscalar(varargin{end})
        varargin = [varargin(1:end - 1), namedargs2cell(varargin{end})];
      end
      parsed = categorical.parseInputs(varargin);
      if parsed.Ordinal && parsed.ProtectedSupplied && ~parsed.Protected
        error('Nelson:categorical:ordinalMustBeProtected', 'Ordinal categorical arrays must be protected.');
      end
      if parsed.Ordinal
        parsed.Protected = true;
      end
      if isa(inputData, 'categorical') && ~parsed.ValueSetSupplied
        obj = inputData;
        if parsed.OrdinalSupplied
          obj.isOrdinal = parsed.Ordinal;
        end
        if parsed.ProtectedSupplied
          obj.isProtected = parsed.Protected;
        end
        return
      end

      preserveExplicitMissing = parsed.ValueSetSupplied && parsed.CategoryNamesSupplied;
      [inputKeys, inputNames, missingMask, inputSize] = categorical.dataKeys(inputData, true, preserveExplicitMissing);
      if parsed.ValueSetSupplied
        [valueKeys, valueNames, ~, ~] = categorical.dataKeys(parsed.ValueSet, false, preserveExplicitMissing);
        if parsed.CategoryNamesSupplied
          categoryNames = categorical.checkCategoryNames(parsed.CategoryNames, true);
          if length(categoryNames) ~= length(valueKeys)
            error('Nelson:categorical:categoryNamesCountMismatch', 'Category names must match the number of values.');
          end
        else
          categoryNames = valueNames;
        end
        [categoryNames, valueToCategory] = categorical.mergeDuplicateCategoryNames(categoryNames);
        codes = categorical.codesFromExplicitValues(inputKeys, missingMask, valueKeys, valueToCategory);
      else
        [categoryNames, valueKeys] = categorical.inferCategories(inputKeys, inputNames, missingMask);
        codes = categorical.codesFromKeys(inputKeys, missingMask, valueKeys);
      end
      obj.codes = reshape(codes, inputSize);
      obj.categoryNames = categoryNames(:);
      obj.isOrdinal = parsed.Ordinal;
      obj.isProtected = parsed.Protected;
    end

    function varargout = size(obj, varargin)
      if nargin == 1
        if nargout <= 1
          varargout{1} = size(obj.codes);
        else
          [varargout{1:nargout}] = size(obj.codes);
        end
      else
        varargout{1} = size(obj.codes, varargin{1});
      end
    end

    function n = numel(obj, varargin)
      if nargin == 1
        n = numel(obj.codes);
      else
        n = numel(obj.codes, varargin{:});
      end
    end

    function n = length(obj)
      n = length(obj.codes);
    end

    function n = ndims(obj)
      n = ndims(obj.codes);
    end

    function tf = isempty(obj)
      tf = isempty(obj.codes);
    end

    function tf = isrow(obj)
      tf = isrow(obj.codes);
    end

    function tf = iscolumn(obj)
      tf = iscolumn(obj.codes);
    end

    function tf = isvector(obj)
      tf = isvector(obj.codes);
    end

    function tf = isscalar(obj)
      tf = isscalar(obj.codes);
    end

    function n = end(obj, index, count)
      sz = size(obj.codes);
      if count == 1
        n = numel(obj.codes);
      elseif index <= length(sz)
        n = sz(index);
      else
        n = 1;
      end
    end

    function out = subsref(obj, S)
      if strcmp(S(1).type, '()')
        out = categorical.fromCodes(obj.codes(S(1).subs{:}), obj.categoryNames, obj.isOrdinal, obj.isProtected);
        if length(S) > 1
          out = subsref(out, S(2:end));
        end
        return
      end
      if strcmp(S(1).type, '{}')
        error('Nelson:categorical:braceIndexingNotSupported', 'Brace indexing is not supported for categorical arrays.');
      end
      if strcmp(S(1).type, '.')
        switch S(1).subs
          case 'codes'
            out = obj.codes;
          case 'categoryNames'
            out = obj.categoryNames;
          case 'isProtected'
            out = obj.isProtected;
          case 'isOrdinal'
            out = obj.isOrdinal;
          otherwise
            error('Nelson:categorical:dotIndexingNotSupported', 'Dot indexing is not supported for categorical arrays.');
        end
        if length(S) > 1
          out = subsref(out, S(2:end));
        end
        return
      end
      error('Nelson:categorical:invalidIndexing', 'Invalid categorical indexing.');
    end

    function obj = subsasgn(obj, S, value)
      if ~strcmp(S(1).type, '()') || length(S) ~= 1
        error('Nelson:categorical:invalidAssignment', 'Invalid categorical assignment.');
      end
      if isnumeric(value) && isempty(value)
        obj.codes(S(1).subs{:}) = [];
        return
      end
      if ischar(value)
        value = {value};
      end
      if ~isa(value, 'categorical')
        value = categorical(value);
      end
      [rhsCodes, names] = categorical.convertCodesForAssignment(obj, value);
      if length(names) > length(obj.categoryNames)
        obj.codes = double(obj.codes);
      end
      obj.codes(S(1).subs{:}) = rhsCodes;
      obj.categoryNames = names;
    end

    function out = reshape(obj, varargin)
      out = categorical.fromCodes(reshape(obj.codes, varargin{:}), obj.categoryNames, obj.isOrdinal, obj.isProtected);
    end

    function out = permute(obj, order)
      out = categorical.fromCodes(permute(obj.codes, order), obj.categoryNames, obj.isOrdinal, obj.isProtected);
    end

    function out = transpose(obj)
      out = categorical.fromCodes(obj.codes.', obj.categoryNames, obj.isOrdinal, obj.isProtected);
    end

    function out = ctranspose(obj)
      out = transpose(obj);
    end

    function c = cellstr(obj)
      c = categorical.cellText(obj);
    end

    function s = string(obj)
      s = string(categorical.cellText(obj));
      % <undefined> elements (code 0) convert to string missing
      mask = isundefined(obj);
      if any(mask(:))
        s(mask) = string(NaN);
      end
    end

    function c = char(obj)
      c = char(categorical.cellText(obj));
    end

    function disp(obj)
      fprintf('%s', categorical.displayText(obj, ''));
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      fprintf('%s', [categorical.displayHeader(obj, name), categorical.displayText(obj, name)]);
    end
  end

  methods (Hidden)
    texts = compactElementText(obj)
  end

  methods (Static)
    function obj = empty(varargin)
      obj = categorical.fromCodes(double.empty(varargin{:}), {}, false, false);
    end

    function obj = fromCodes(codes, names, isOrdinal, isProtected)
      obj = categorical();
      obj.codes = double(codes);
      obj.categoryNames = names(:);
      obj.isOrdinal = logical(isOrdinal);
      obj.isProtected = logical(isProtected);
    end

    function parsed = parseInputs(args)
      parsed = struct('ValueSetSupplied', false, 'CategoryNamesSupplied', false, ...
        'ValueSet', [], 'CategoryNames', [], 'Ordinal', false, 'Protected', false, ...
        'OrdinalSupplied', false, 'ProtectedSupplied', false);
      firstOption = length(args) + 1;
      for k = 1:length(args)
        if categorical.isOptionName(args{k})
          firstOption = k;
          break
        end
      end
      positional = args(1:firstOption - 1);
      options = args(firstOption:end);
      if length(positional) >= 1
        parsed.ValueSetSupplied = true;
        parsed.ValueSet = positional{1};
      end
      if length(positional) >= 2
        parsed.CategoryNamesSupplied = true;
        parsed.CategoryNames = positional{2};
      end
      if length(positional) > 2 || mod(length(options), 2) ~= 0
        error('Nelson:categorical:invalidInputArguments', 'Invalid categorical input arguments.');
      end
      k = 1;
      while k <= length(options)
        name = char(options{k});
        value = logical(options{k + 1});
        if strcmpi(name, 'Ordinal')
          parsed.Ordinal = value;
          parsed.OrdinalSupplied = true;
        elseif strcmpi(name, 'Protected')
          parsed.Protected = value;
          parsed.ProtectedSupplied = true;
        else
          error('Nelson:categorical:invalidOption', 'Invalid %s option.', 'categorical');
        end
        k = k + 2;
      end
    end

    function tf = isOptionName(value)
      tf = false;
      if isstring(value)
        if ~isscalar(value)
          return
        end
        value = char(value);
      end
      if ischar(value)
        tf = strcmpi(value, 'Ordinal') || strcmpi(value, 'Protected');
      end
    end

    function [keys, names, missing, dims] = dataKeys(value, rejectChar, preserveMissing)
      if nargin < 3
        preserveMissing = false;
      end
      if isa(value, 'categorical')
        dims = size(value.codes);
        codes = value.codes(:);
        keys = cell(length(codes), 1);
        names = keys;
        missing = codes == 0;
        for k = 1:length(codes)
          if missing(k)
            keys{k} = '';
            names{k} = '';
          else
            names{k} = value.categoryNames{codes(k)};
            keys{k} = ['text:', names{k}];
          end
        end
        return
      end
      if ischar(value)
        if rejectChar
          error('Nelson:categorical:charMustBeWrapped', 'Character arrays must be wrapped in a cell array or string array.');
        end
        value = {value};
      end
      dims = size(value);
      if iscellstr(value)
        raw = strtrim(value(:));
        missing = ismissing(value(:));
        keys = strcat('text:', raw);
        names = raw;
      elseif isstring(value)
        missing = ismissing(value(:));
        stringValue = value(:);
        if any(missing)
          stringValue(missing) = "";
        end
        raw = strtrim(cellstr(stringValue));
        missing = missing | strcmp(raw, '');
        keys = strcat('text:', raw);
        names = raw;
      elseif isnumeric(value)
        flat = double(value(:));
        missing = ismissing(flat);
        keys = cell(length(flat), 1);
        names = cell(length(flat), 1);
        for k = 1:length(flat)
          names{k} = categorical.numberName(flat(k));
          keys{k} = ['number:', names{k}];
        end
      elseif islogical(value)
        flat = logical(value(:));
        missing = false(length(flat), 1);
        keys = cell(length(flat), 1);
        names = cell(length(flat), 1);
        for k = 1:length(flat)
          if flat(k)
            names{k} = 'true';
          else
            names{k} = 'false';
          end
          keys{k} = ['logical:', names{k}];
        end
      else
        try
          raw = strtrim(cellstr(string(value(:))));
        catch
          error('Nelson:categorical:unsupportedInputType', 'Unsupported categorical input type.');
        end
        missing = ismissing(value(:)) | strcmp(raw, '') | strcmp(raw, '<missing>') | strcmp(raw, 'NaT');
        keys = strcat(['object:', class(value), ':'], raw);
        names = raw;
      end
      if ~preserveMissing
        keys(missing) = {''};
        names(missing) = {''};
      else
        missing(:) = false;
      end
    end

    function name = numberName(value)
      if isnan(value)
        name = '';
      elseif isinf(value)
        if value > 0
          name = 'Inf';
        else
          name = '-Inf';
        end
      elseif value == round(value)
        name = num2str(value, '%.0f');
      else
        name = num2str(value, '%.15g');
      end
    end

    function names = checkCategoryNames(names, allowDuplicates)
      if isstring(names)
        names = cellstr(names);
      elseif ischar(names)
        names = {names};
      end
      if ~iscellstr(names)
        error('Nelson:categorical:categoryNamesMustBeText', '%s must be text.', 'Category names');
      end
      names = strtrim(names(:));
      for k = 1:length(names)
        if isempty(names{k}) || strcmp(names{k}, '<undefined>') || strcmp(names{k}, '<missing>')
          error('Nelson:categorical:invalidCategoryName', 'Invalid category name.');
        end
      end
      if ~allowDuplicates && length(unique(names)) ~= length(names)
        error('Nelson:categorical:categoryNamesMustBeUnique', 'Category names must be unique.');
      end
    end

    function [names, valueToCategory] = mergeDuplicateCategoryNames(names)
      valueToCategory = zeros(length(names), 1);
      uniqueNames = {};
      for k = 1:length(names)
        pos = find(strcmp(uniqueNames, names{k}), 1);
        if isempty(pos)
          uniqueNames{end + 1, 1} = names{k};
          pos = length(uniqueNames);
        end
        valueToCategory(k) = pos;
      end
      names = uniqueNames(:);
    end

    function [names, categoryKeys] = inferCategories(keys, displayNames, missing)
      categoryKeys = unique(keys(~missing));
      categoryKeys(strcmp(categoryKeys, '')) = [];
      % Map every key to its category once (ismember is O(n) via a hash); the
      % former find(strcmp(...)) per category was O(nCategories * nKeys).
      [~, categoryOfKey] = ismember(keys(:), categoryKeys(:));
      categoryOfKey = categoryOfKey(:);
      names = cell(length(categoryKeys), 1);
      firstKey = zeros(length(categoryKeys), 1);
      present = find(categoryOfKey > 0);
      % Assign in reverse so the earliest key of each category wins.
      firstKey(categoryOfKey(present(end:-1:1))) = present(end:-1:1);
      if ~isempty(categoryKeys)
        names = displayNames(firstKey);
        names = names(:);
      end
    end

    function codes = codesFromKeys(keys, missing, categoryKeys)
      % ismember maps all keys at once; the former find(strcmp(...)) per key
      % was O(nKeys * nCategories). categoryKeys is unique, so the match index
      % is unambiguous.
      [~, codes] = ismember(keys(:), categoryKeys(:));
      codes = codes(:);
      codes(missing(:)) = 0;
    end

    function codes = codesFromExplicitValues(keys, missing, valueKeys, valueToCategory)
      codes = zeros(length(keys), 1);
      for k = 1:length(keys)
        if ~missing(k)
          idx = find(strcmp(valueKeys, keys{k}), 1);
          if ~isempty(idx)
            codes(k) = valueToCategory(idx);
          end
        end
      end
    end

    function [codes, names] = convertCodesForAssignment(left, right)
      if left.isOrdinal ~= right.isOrdinal
        error('Nelson:categorical:ordinalFlagsMismatch', 'Categorical ordinal flags must match.');
      end
      names = left.categoryNames;
      if left.isOrdinal && ~isequal(names, right.categoryNames)
        error('Nelson:categorical:identicalCategoriesRequired', 'Ordinal categorical arrays must have identical categories.');
      end
      map = zeros(length(right.categoryNames), 1);
      for k = 1:length(right.categoryNames)
        idx = find(strcmp(names, right.categoryNames{k}), 1);
        if isempty(idx)
          if left.isProtected
            error('Nelson:categorical:cannotAddToProtected', 'Cannot add a category to a protected categorical array.');
          end
          names{end + 1, 1} = right.categoryNames{k};
          idx = length(names);
        end
        map(k) = idx;
      end
      rc = right.codes;
      codes = zeros(size(rc));
      nz = rc > 0;
      codes(nz) = map(rc(nz));
    end

    function [leftCodes, rightCodes] = reconcile(left, right, requireOrdinal)
      if ischar(left)
        left = {left};
      end
      if ischar(right)
        right = {right};
      end
      leftIsCategorical = isa(left, 'categorical');
      rightIsCategorical = isa(right, 'categorical');
      if leftIsCategorical && ~rightIsCategorical && (iscellstr(right) || isstring(right))
        right = categorical(right, left.categoryNames, left.categoryNames, ...
          'Ordinal', left.isOrdinal, 'Protected', left.isProtected);
      elseif rightIsCategorical && ~leftIsCategorical && (iscellstr(left) || isstring(left))
        left = categorical(left, right.categoryNames, right.categoryNames, ...
          'Ordinal', right.isOrdinal, 'Protected', right.isProtected);
      end
      if ~isa(left, 'categorical')
        left = categorical(left);
      end
      if ~isa(right, 'categorical')
        right = categorical(right);
      end
      if requireOrdinal && (~left.isOrdinal || ~right.isOrdinal)
        error('Nelson:categorical:relationalRequiresOrdinal', 'Relational comparisons require ordinal categorical arrays.');
      end
      if left.isOrdinal ~= right.isOrdinal
        error('Nelson:categorical:ordinalFlagsMismatch', 'Categorical ordinal flags must match.');
      end
      leftCodes = left.codes;
      if isequal(left.categoryNames, right.categoryNames)
        rightCodes = right.codes;
        return
      end
      if left.isOrdinal || requireOrdinal
        error('Nelson:categorical:identicalCategoriesRequired', 'Ordinal categorical arrays must have identical categories.');
      end
      map = zeros(length(right.categoryNames), 1);
      for k = 1:length(right.categoryNames)
        idx = find(strcmp(left.categoryNames, right.categoryNames{k}), 1);
        if ~isempty(idx)
          map(k) = idx;
        end
      end
      rc = right.codes;
      rightCodes = zeros(size(rc));
      nz = rc > 0;
      rightCodes(nz) = map(rc(nz));
    end

    function values = cellText(obj)
      % Hoist the property accesses out of the loop: reading obj.codes or
      % obj.categoryNames inside the loop copies the whole array each
      % iteration, which makes the conversion O(numel^2).
      codes = obj.codes;
      names = obj.categoryNames;
      n = numel(codes);
      values = cell(size(codes));
      for k = 1:n
        code = codes(k);
        if code == 0
          values{k} = '<undefined>';
        else
          values{k} = names{code};
        end
      end
    end

    function text = displayText(obj, name)
      % body shared by disp and display: rows, or one block per page
      values = categorical.cellText(obj);
      if isempty(values)
        text = '';
        return
      end
      blank = categorical.displayBlankLine();
      values = reshape(values, size(obj.codes));
      if ismatrix(values)
        text = [categorical.displayMatrixText(values, blank), blank];
        return
      end
      text = '';
      pages = size(values);
      for page = 1:prod(pages(3:end))
        subs = cell(1, length(pages) - 2);
        [subs{:}] = ind2sub(pages(3:end), page);
        pageSubs = [{':'}, {':'}, subs];
        header = [name, '(:,:,', int2str(page), ') = ', newline];
        text = [text, blank, header, blank, categorical.displayMatrixText(values(pageSubs{:}), blank), blank];
      end
    end

    function text = displayHeader(obj, name)
      % name = (2-D or empty only), then the size line; pages always start
      % with a blank line, named or not
      blank = categorical.displayBlankLine();
      isPaged = ~ismatrix(obj.codes) && ~isempty(obj.codes);
      text = '';
      if isPaged
        text = blank;
      elseif ~isempty(name)
        text = [blank, name, ' = ', newline, blank];
      end
      dims = strjoin(arrayfun(@int2str, size(obj.codes), 'UniformOutput', false), char(215));
      if isempty(obj.codes)
        sizeLine = ['  ', dims, ' empty categorical array'];
      elseif isscalar(obj.codes)
        sizeLine = '  categorical';
      else
        sizeLine = ['  ', dims, ' categorical array'];
      end
      text = [text, sizeLine, newline];
      if ~isPaged
        text = [text, blank];
      end
    end

    function blank = displayBlankLine()
      currentFormat = format();
      if strcmp(currentFormat.LineSpacing, 'loose')
        blank = newline;
      else
        blank = '';
      end
    end

    function text = displayMatrixText(values, blank)
      % elements left-justified to the column width (one column: no padding
      % nor truncation); wide rows are split in column groups
      [rows, cols] = size(values);
      [~, width] = terminal_size();
      widths = zeros(1, cols);
      if cols > 1
        maxLength = max(19, floor(width / cols) - 7);
        tooLong = cellfun('length', values) > maxLength;
        values(tooLong) = cellfun(@(v) [v(1:maxLength - 1), char(8230)], values(tooLong), 'UniformOutput', false);
        widths = max(cellfun('length', values), [], 1);
      end
      groups = categorical.columnGroups(widths + 6, width);
      text = '';
      for g = 1:size(groups, 1)
        first = groups(g, 1);
        last = groups(g, 2);
        if size(groups, 1) > 1
          if g > 1
            text = [text, blank];
          end
          text = [text, categorical.columnsHeader(first, last), newline, blank];
        end
        for row = 1:rows
          line = '';
          for col = first:last
            value = values{row, col};
            line = [line, '     ', value, blanks(widths(col) - length(value)), ' '];
          end
          text = [text, line, newline];
        end
      end
    end

    function groups = columnGroups(elementWidths, width)
      % [first, last] rows: as many columns as fit before the line reaches width
      groups = zeros(0, 2);
      first = 1;
      cols = length(elementWidths);
      while first <= cols
        fits = find(cumsum(elementWidths(first:end)) <= width - 1, 1, 'last');
        if isempty(fits)
          fits = 1;
        end
        groups(end + 1, :) = [first, first + fits - 1];
        first = first + fits;
      end
    end

    function header = columnsHeader(first, last)
      if first == last
        header = ['  ', getString(message('Nelson:display_format:column', first))];
      else
        header = ['  ', getString(message('Nelson:display_format:columnsThrough', first, last))];
      end
    end

    function [A, B] = expandPairCodes(A, B)
      if isscalar(A)
        A = repmat(A, size(B));
      elseif isscalar(B)
        B = repmat(B, size(A));
      elseif ~isequal(size(A), size(B))
        error('Nelson:dimagree:valueCompatibleSizes', '%s must have compatible sizes.', 'Categorical arrays');
      end
    end

    function [tf, loc] = namesMember(names, universe)
      if ischar(names)
        names = {names};
      end
      if isstring(names)
        names = cellstr(names);
      end
      tf = false(size(names));
      loc = zeros(size(names));
      for k = 1:numel(names)
        idx = find(strcmp(universe, names{k}), 1);
        if ~isempty(idx)
          tf(k) = true;
          loc(k) = idx;
        end
      end
    end

    function codes = categoryCodes(names, universe)
      if isa(names, 'pattern')
        matched = regexp(universe, pattern.regexOf(names), 'once');
        codes = find(~cellfun('isempty', matched));
        return
      end
      names = categorical.checkCategoryNames(names, true);
      [tf, codes] = categorical.namesMember(names, universe);
      codes = codes(tf);
    end

    function expr = intervalExpression(intervals, len)
      parts = cell(1, size(intervals, 1));
      for k = 1:size(intervals, 1)
        first = intervals(k, 1);
        last = intervals(k, 2);
        if first == 1 && last == len
          parts{k} = ':';
        elseif first == last
          parts{k} = int2str(first);
        else
          parts{k} = [int2str(first), ':', int2str(last)];
        end
      end
      if length(parts) == 1
        expr = parts{1};
      else
        expr = ['[', strjoin(parts, ','), ']'];
      end
    end
  end
end
%=============================================================================
