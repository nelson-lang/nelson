%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef inputParser < handle
  properties (Dependent)
    FunctionName
    CaseSensitive
    KeepUnmatched
    PartialMatching
    StructExpand
    Parameters
    Results
    Unmatched
    UsingDefaults
  end

  properties (Hidden)
    FunctionNameValue = ''
    CaseSensitiveValue = false
    KeepUnmatchedValue = false
    PartialMatchingValue = true
    StructExpandValue = true
    Schema = struct('Name', {}, 'Type', {}, 'Default', {}, 'Validator', {})
    ResultsValue = struct()
    UnmatchedValue = struct()
    UsingDefaultsValue = {}
  end

  methods
    function obj = inputParser()
      nargoutchk(0, 1);
    end

    function value = get.FunctionName(obj)
      value = obj.FunctionNameValue;
    end

    function set.FunctionName(obj, value)
      obj.FunctionNameValue = inputParser.textScalar(value, 'FunctionName');
    end

    function value = get.CaseSensitive(obj)
      value = obj.CaseSensitiveValue;
    end

    function set.CaseSensitive(obj, value)
      obj.CaseSensitiveValue = inputParser.logicalScalar(value, 'CaseSensitive');
    end

    function value = get.KeepUnmatched(obj)
      value = obj.KeepUnmatchedValue;
    end

    function set.KeepUnmatched(obj, value)
      obj.KeepUnmatchedValue = inputParser.logicalScalar(value, 'KeepUnmatched');
    end

    function value = get.PartialMatching(obj)
      value = obj.PartialMatchingValue;
    end

    function set.PartialMatching(obj, value)
      obj.PartialMatchingValue = inputParser.logicalScalar(value, 'PartialMatching');
    end

    function value = get.StructExpand(obj)
      value = obj.StructExpandValue;
    end

    function set.StructExpand(obj, value)
      obj.StructExpandValue = inputParser.logicalScalar(value, 'StructExpand');
    end

    function value = get.Parameters(obj)
      value = {};
      for k = 1:numel(obj.Schema)
        value{end + 1} = obj.Schema(k).Name;
      end
    end

    function value = get.Results(obj)
      value = obj.ResultsValue;
    end

    function value = get.Unmatched(obj)
      value = obj.UnmatchedValue;
    end

    function value = get.UsingDefaults(obj)
      value = obj.UsingDefaultsValue;
    end

    addRequired(obj, name, validator)
    addOptional(obj, name, defaultValue, validator)
    addParameter(obj, name, defaultValue, validator)
    addParamValue(obj, name, defaultValue, validator)
    parse(obj, varargin)
  end

  methods (Static, Hidden)
    function value = textScalar(value, name)
      if isstring(value) && isscalar(value)
        value = char(value);
      end
      if ~(ischar(value) && isrow(value))
        error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
      end
    end

    function value = logicalScalar(value, name)
      if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
        error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
      end
      value = logical(value);
    end

    function addSchema(obj, kind, name, defaultValue, validator)
      name = inputParser.textScalar(name, 'Argument name');
      if ~isvarname(name)
        error('Nelson:validators:inputParserInvalidName', 'Argument name must be a valid identifier.');
      end
      if ~isempty(inputParser.findSchema(obj, name, false))
        error('Nelson:validators:inputParserDuplicateName', 'Input names must be unique.');
      end
      item.Name = name;
      item.Type = kind;
      item.Default = defaultValue;
      item.Validator = validator;
      obj.Schema(end + 1) = item;
    end

    function index = findSchema(obj, name, parametersOnly)
      index = [];
      for k = 1:numel(obj.Schema)
        if parametersOnly && ~strcmp(obj.Schema(k).Type, 'parameter')
          continue
        end
        if inputParser.nameEquals(obj, obj.Schema(k).Name, name)
          index = k;
          return
        end
      end
    end

    function [index, ambiguous] = resolveParameter(obj, name)
      index = [];
      ambiguous = false;
      exact = [];
      partial = [];
      for k = 1:numel(obj.Schema)
        if ~strcmp(obj.Schema(k).Type, 'parameter')
          continue
        end
        if inputParser.nameEquals(obj, obj.Schema(k).Name, name)
          exact = k;
          break
        elseif obj.PartialMatchingValue && inputParser.nameStartsWith(obj, obj.Schema(k).Name, name)
          partial(end + 1) = k;
        end
      end
      if ~isempty(exact)
        index = exact;
      elseif numel(partial) == 1
        index = partial(1);
      elseif numel(partial) > 1
        ambiguous = true;
      end
    end

    function tf = nameEquals(obj, a, b)
      if obj.CaseSensitiveValue
        tf = strcmp(a, b);
      else
        tf = strcmpi(a, b);
      end
    end

    function tf = nameStartsWith(obj, candidate, prefix)
      if length(prefix) > length(candidate)
        tf = false;
        return
      end
      if obj.CaseSensitiveValue
        tf = strcmp(candidate(1:length(prefix)), prefix);
      else
        tf = strcmpi(candidate(1:length(prefix)), prefix);
      end
    end

    function tf = isTextScalar(value)
      tf = (ischar(value) && isrow(value)) || (isstring(value) && isscalar(value));
    end

    function validateValue(item, value)
      if isempty(item.Validator)
        return
      end
      try
        try
          result = feval(item.Validator, value);
        catch ex
          if strcmp(ex.identifier, 'Nelson:tooManyOutputs') || strcmp(ex.identifier, 'Nelson:nargoutchk:tooManyOutputs')
            feval(item.Validator, value);
            result = true;
          else
            rethrow(ex);
          end
        end
        if ~isempty(result) && islogical(result) && isscalar(result) && ~result
          error('Nelson:inputParser:validationFunctionReturnedFalse2', 'Validation function returned false.');
        end
      catch ex
        error('Nelson:validators:inputParserInvalidValue', 'The value of ''%s'' is invalid. %s', item.Name, ex.message);
      end
    end

    function tf = optionalAccepts(item, value)
      try
        inputParser.validateValue(item, value);
        tf = true;
      catch
        tf = false;
      end
    end
  end
end
%=============================================================================
