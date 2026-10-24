%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef RandStream < handle
  properties (GetAccess = public, SetAccess = protected)
    Type = 'mt19937ar'
    Seed = uint32(0)
    NumStreams = 1
    StreamIndex = 1
  end

  properties (Dependent)
    Substream
    NormalTransform
    Antithetic
    FullPrecision
    State
  end

  properties (Access = private)
    InternalType = 'twister'
    StateStruct = struct('Type', 'twister', 'Seed', uint32(0), 'State', uint32([]))
    InternalSubstream = 1
    InternalNormalTransform = 'Ziggurat'
    InternalAntithetic = false
    InternalFullPrecision = true
  end

  methods
    function obj = RandStream(type, varargin)
      narginchk(1, Inf);
      [obj.Type, obj.InternalType] = RandStream.resolveType(type);
      opts = RandStream.parseOptions(varargin, obj.InternalType);
      obj.Seed = opts.Seed;
      obj.InternalSubstream = opts.Substream;
      obj.InternalNormalTransform = opts.NormalTransform;
      obj.InternalAntithetic = opts.Antithetic;
      obj.InternalFullPrecision = opts.FullPrecision;
      if isempty(opts.State)
        obj.StateStruct = RandStream.initialState(obj.InternalType, obj.Seed, ...
          obj.InternalSubstream, obj.InternalFullPrecision);
      else
        obj.StateStruct = struct('Type', obj.InternalType, 'Seed', obj.Seed, 'State', opts.State);
      end
    end
    %=========================================================================
    function value = get.State(obj)
      if RandStream.globalStream('iscurrent', obj)
        obj.StateStruct = rng();
        obj.Seed = obj.StateStruct.Seed;
      end
      value = obj.StateStruct.State;
    end
    %=========================================================================
    function set.State(obj, value)
      obj.StateStruct.State = value;
      if RandStream.globalStream('iscurrent', obj)
        rng(obj.StateStruct);
      end
    end
    %=========================================================================
    function value = get.Substream(obj)
      value = obj.InternalSubstream;
    end
    %=========================================================================
    function set.Substream(obj, value)
      obj.InternalSubstream = RandStream.positiveInteger(value, 'Substream');
      obj.StateStruct = RandStream.initialState(obj.InternalType, obj.Seed, ...
        obj.InternalSubstream, obj.InternalFullPrecision);
      if RandStream.globalStream('iscurrent', obj)
        rng(obj.StateStruct);
      end
    end
    %=========================================================================
    function value = get.NormalTransform(obj)
      value = obj.InternalNormalTransform;
    end
    %=========================================================================
    function set.NormalTransform(obj, value)
      obj.InternalNormalTransform = RandStream.normalTransformValue(value);
    end
    %=========================================================================
    function value = get.Antithetic(obj)
      value = obj.InternalAntithetic;
    end
    %=========================================================================
    function set.Antithetic(obj, value)
      obj.InternalAntithetic = RandStream.logicalScalar(value, 'Antithetic');
    end
    %=========================================================================
    function value = get.FullPrecision(obj)
      value = obj.InternalFullPrecision;
    end
    %=========================================================================
    function set.FullPrecision(obj, value)
      obj.InternalFullPrecision = RandStream.logicalScalar(value, 'FullPrecision');
      if RandStream.hasPrecisionMode(obj.InternalType)
        if RandStream.globalStream('iscurrent', obj)
          obj.StateStruct = rng();
        end
        obj.StateStruct.State = RandStream.withPrecisionMode(obj.StateStruct.State, ...
          obj.InternalFullPrecision);
        if RandStream.globalStream('iscurrent', obj)
          rng(obj.StateStruct);
        end
      end
    end
    %=========================================================================
    function reset(obj, varargin)
      if nargin > 2
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      if nargin == 2
        obj.Seed = RandStream.seedValue(varargin{1});
      end
      obj.StateStruct = RandStream.initialState(obj.InternalType, obj.Seed, ...
        obj.InternalSubstream, obj.InternalFullPrecision);
      if RandStream.globalStream('iscurrent', obj)
        rng(obj.StateStruct);
      end
    end
    %=========================================================================
    function varargout = rand(obj, varargin)
      n = max(nargout, 1);
      varargout = RandStream.generate(obj, 'rand', varargin, n);
    end
    %=========================================================================
    function varargout = randn(obj, varargin)
      n = max(nargout, 1);
      varargout = RandStream.generate(obj, 'randn', varargin, n);
    end
    %=========================================================================
    function varargout = randi(obj, varargin)
      n = max(nargout, 1);
      varargout = RandStream.generate(obj, 'randi', varargin, n);
    end
    %=========================================================================
    function varargout = randperm(obj, varargin)
      n = max(nargout, 1);
      varargout = RandStream.generate(obj, 'randperm', varargin, n);
    end
    %=========================================================================
    function varargout = rng(obj)
      backup = rng();
      if ~RandStream.globalStream('iscurrent', obj)
        % the global stream state lives in the global generator
        rng(obj.StateStruct);
      end
      if nargout > 0
        varargout{1} = backup;
      end
    end
    %=========================================================================
    function data = saveobj(obj)
      data = struct();
      data.Type = obj.Type;
      data.Seed = obj.Seed;
      data.NumStreams = obj.NumStreams;
      data.StreamIndex = obj.StreamIndex;
      data.Substream = obj.Substream;
      data.NormalTransform = obj.NormalTransform;
      data.Antithetic = obj.Antithetic;
      data.FullPrecision = obj.FullPrecision;
      data.State = obj.State;
    end
    %=========================================================================
    function varargout = horzcat(varargin)
      error('Nelson:RandStream:cannotConcatenate', 'RandStream objects cannot be concatenated.');
    end
    %=========================================================================
    function varargout = vertcat(varargin)
      error('Nelson:RandStream:cannotConcatenate', 'RandStream objects cannot be concatenated.');
    end
    %=========================================================================
    function varargout = cat(varargin)
      error('Nelson:RandStream:cannotConcatenate', 'RandStream objects cannot be concatenated.');
    end
  end

  methods (Hidden)
    function syncFromGlobalState(obj)
      obj.StateStruct = rng();
      obj.Seed = obj.StateStruct.Seed;
    end
  end

  methods (Static)
    function [varargout] = create(type, varargin)
      narginchk(1, Inf);
      opts = RandStream.parseCreateOptions(varargin);
      indices = opts.StreamIndices;
      if isempty(indices)
        indices = 1:opts.NumStreams;
      end
      streams = cell(1, length(indices));
      for k = 1:length(indices)
        streams{k} = RandStream(type, 'Seed', opts.Seed, 'Substream', indices(k));
        streams{k}.NumStreams = opts.NumStreams;
        streams{k}.StreamIndex = indices(k);
        streams{k}.InternalSubstream = 1;
      end
      if opts.CellOutput
        if nargout > 1
          error('Nelson:tooManyOutputs', 'Too many output arguments.');
        end
        varargout{1} = streams;
      else
        requestedOutputs = max(nargout, 1);
        if requestedOutputs > length(streams)
          error('Nelson:tooManyOutputs', 'Too many output arguments.');
        end
        for k = 1:requestedOutputs
          varargout{k} = streams{k};
        end
      end
    end
    %=========================================================================
    function list()
      if nargout > 0
        error('Nelson:tooManyOutputs', 'Too many output arguments.');
      end
      disp('The following random number generator algorithms are available:');
      disp('dsfmt19937');
      disp('mcg16807');
      disp('mlfg6331_64');
      disp('mrg32k3a');
      disp('mt19937ar');
      disp('pcg64dxsm');
      disp('philox4x32_10');
      disp('shr3cong');
      disp('swb2712');
      disp('threefry4x64_20');
      disp('twister64');
      disp('xoshiro256pp');
    end
    %=========================================================================
    function stream = getGlobalStream()
      stream = RandStream.globalStream('get');
      if isempty(stream)
        stream = RandStream.fromState(rng());
        RandStream.globalStream('set', stream);
      else
        stream.syncFromGlobalState();
      end
    end
    %=========================================================================
    function previous = setGlobalStream(stream)
      if ~(isa(stream, 'RandStream') && isscalar(stream))
        error('Nelson:RandStream:scalarObjectExpected', '%s must be a scalar RandStream object.', 'Input');
      end
      previous = RandStream.getGlobalStream();
      rng(stream.StateStruct);
      RandStream.globalStream('set', stream);
    end
  end

  methods (Hidden, Static)
    function obj = loadobj(data)
      if isa(data, 'RandStream')
        obj = data;
        return
      end
      obj = RandStream(data.Type, 'Seed', data.Seed, 'State', data.State);
      obj.NumStreams = data.NumStreams;
      obj.StreamIndex = data.StreamIndex;
      obj.InternalSubstream = data.Substream;
      obj.NormalTransform = data.NormalTransform;
      obj.Antithetic = data.Antithetic;
      obj.FullPrecision = data.FullPrecision;
    end
    %=========================================================================
    function varargout = empty(varargin)
      error('Nelson:RandStream:emptyArrayNotSupported', 'RandStream empty arrays are not supported.');
    end
  end

  methods (Static, Access = private)
    function out = generate(obj, functionName, args, n)
      if ~(isa(obj, 'RandStream') && isscalar(obj))
        error('Nelson:RandStream:scalarObjectExpected', '%s must be a scalar RandStream object.', 'First input');
      end
      if strcmp(functionName, 'randn') && strcmp(obj.InternalNormalTransform, 'Inversion') ...
          && ~RandStream.hasPrecisionMode(obj.InternalType)
        % Inversion transform: apply the inverse normal CDF to uniform draws
        % (pcg and xoshiro engines already generate normal values this way).
        u = RandStream.draw(obj, 'rand', args, 1);
        u = u{1};
        if isreal(u)
          out = {norminv(u)};
        else
          out = {complex(norminv(real(u)), norminv(imag(u)))};
        end
      else
        out = RandStream.draw(obj, functionName, args, n);
      end
      if strcmp(functionName, 'rand') && obj.InternalAntithetic
        for k = 1:length(out)
          out{k} = 1 - out{k};
        end
      end
    end
    %=========================================================================
    function out = draw(obj, functionName, args, n)
      % The global stream draws from the global generator; any other stream
      % draws from a private engine built from its state, so the global
      % generator is never read nor modified.
      out = cell(1, n);
      if RandStream.globalStream('iscurrent', obj)
        if strcmp(functionName, 'randperm')
          [out{1:n}] = randperm(args{:});
        else
          [out{1:n}] = builtin(functionName, args{:});
        end
      else
        [obj.StateStruct, out{1:n}] = __rng_eval__(obj.StateStruct, functionName, args{:});
      end
    end
    %=========================================================================
    function stream = fromState(state)
      compatibleType = RandStream.compatibleTypeFromInternal(state.Type);
      stream = RandStream(compatibleType, 'Seed', state.Seed, 'State', state.State);
      stream.StateStruct = state;
      if RandStream.hasPrecisionMode(state.Type)
        stream.InternalFullPrecision = (state.State(end - 1) == 0);
      end
    end
    %=========================================================================
    function state = initialState(internalType, seed, substream, fullPrecision)
      state = struct('Type', internalType, 'Seed', seed, 'State', []);
      if ~fullPrecision && RandStream.hasPrecisionMode(internalType)
        state = __rng_eval__(state);
        state.State = RandStream.withPrecisionMode(state.State, false);
      end
      if substream > 1
        state = __rng_eval__(state, 'rand', 1, substream - 1);
      else
        state = __rng_eval__(state);
      end
    end
    %=========================================================================
    function tf = hasPrecisionMode(internalType)
      % pcg and xoshiro engines: uint64 state [engine words; precision mode; cached half].
      tf = any(strcmp(internalType, {'pcg', 'xoshiro'}));
    end
    %=========================================================================
    function state = withPrecisionMode(state, fullPrecision)
      state(end - 1) = uint64(~fullPrecision);
      state(end) = uint64(0);
    end
    %=========================================================================
    function value = globalStream(action, stream)
      persistent current;
      value = [];
      switch action
        case 'set'
          current = stream;
        case 'get'
          value = current;
        case 'iscurrent'
          value = ~isempty(current) && (current == stream);
        otherwise
          error('Nelson:RandStream:invalidGlobalStreamAction', 'Invalid RandStream global stream action.');
      end
    end
    %=========================================================================
    function opts = parseOptions(args, internalType)
      defaultTransform = 'Ziggurat';
      if RandStream.hasPrecisionMode(internalType)
        defaultTransform = 'Inversion';
      end
      opts = struct('Seed', uint32(0), 'State', [], 'Substream', 1, ...
        'NormalTransform', defaultTransform, 'Antithetic', false, 'FullPrecision', true);
      if rem(length(args), 2) ~= 0
        error('Nelson:RandStream:nameValueNotPaired', 'Name-value arguments must be paired.');
      end
      for k = 1:2:length(args)
        name = RandStream.optionName(args{k}, ...
          {'seed', 'state', 'substream', 'normaltransform', 'antithetic', 'fullprecision'});
        switch name
          case 'seed'
            opts.Seed = RandStream.seedValue(args{k + 1});
          case 'state'
            opts.State = args{k + 1};
          case 'substream'
            opts.Substream = RandStream.positiveInteger(args{k + 1}, 'Substream');
          case 'normaltransform'
            opts.NormalTransform = RandStream.normalTransformValue(args{k + 1});
          case 'antithetic'
            opts.Antithetic = RandStream.logicalScalar(args{k + 1}, 'Antithetic');
          case 'fullprecision'
            opts.FullPrecision = RandStream.logicalScalar(args{k + 1}, 'FullPrecision');
        end
      end
    end
    %=========================================================================
    function opts = parseCreateOptions(args)
      opts = struct('Seed', uint32(0), 'NumStreams', 1, 'StreamIndices', [], ...
        'CellOutput', false);
      if rem(length(args), 2) ~= 0
        error('Nelson:RandStream:nameValueNotPaired', 'Name-value arguments must be paired.');
      end
      for k = 1:2:length(args)
        name = RandStream.optionName(args{k}, ...
          {'seed', 'numstreams', 'streamindices', 'celloutput'});
        switch name
          case 'seed'
            opts.Seed = RandStream.seedValue(args{k + 1});
          case 'numstreams'
            opts.NumStreams = RandStream.positiveInteger(args{k + 1}, 'NumStreams');
          case 'streamindices'
            opts.StreamIndices = RandStream.indexVector(args{k + 1}, 'StreamIndices');
          case 'celloutput'
            opts.CellOutput = RandStream.logicalScalar(args{k + 1}, 'CellOutput');
        end
      end
      if ~isempty(opts.StreamIndices) && any(opts.StreamIndices > opts.NumStreams)
        error('Nelson:RandStream:streamIndicesOutOfRange', 'StreamIndices must be less than or equal to NumStreams.');
      end
    end
    %=========================================================================
    function [compatibleType, internalType] = resolveType(type)
      if ~(ischar(type) || (isstring(type) && isscalar(type)))
        error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Generator type');
      end
      text = lower(char(type));
      switch text
        case {'mt19937ar', 'twister'}
          compatibleType = 'mt19937ar';
          internalType = 'twister';
        case 'twister64'
          compatibleType = 'twister64';
          internalType = 'twister64';
        case {'dsfmt19937', 'simdtwister'}
          compatibleType = 'dsfmt19937';
          internalType = 'simdTwister';
        case {'mrg32k3a', 'combrecursive'}
          compatibleType = 'mrg32k3a';
          internalType = 'combRecursive';
        case {'mlfg6331_64', 'laggedfibonacci607'}
          compatibleType = 'mlfg6331_64';
          internalType = 'laggedfibonacci607';
        case {'philox4x32_10', 'philox'}
          compatibleType = 'philox4x32_10';
          internalType = 'philox';
        case {'threefry4x64_20', 'threefry'}
          compatibleType = 'threefry4x64_20';
          internalType = 'threefry';
        case {'pcg64dxsm', 'pcg'}
          compatibleType = 'pcg64dxsm';
          internalType = 'pcg';
        case {'xoshiro256pp', 'xoshiro'}
          compatibleType = 'xoshiro256pp';
          internalType = 'xoshiro';
        case {'mcg16807', 'shr3cong', 'swb2712'}
          compatibleType = text;
          internalType = 'twister';
        otherwise
          error('Nelson:RandStream:invalidGeneratorType', 'A valid generator type expected.');
      end
    end
    %=========================================================================
    function compatibleType = compatibleTypeFromInternal(type)
      switch lower(char(type))
        case 'twister'
          compatibleType = 'mt19937ar';
        case 'twister64'
          compatibleType = 'twister64';
        case 'simdtwister'
          compatibleType = 'dsfmt19937';
        case 'combrecursive'
          compatibleType = 'mrg32k3a';
        case 'laggedfibonacci607'
          compatibleType = 'mlfg6331_64';
        case 'philox'
          compatibleType = 'philox4x32_10';
        case 'threefry'
          compatibleType = 'threefry4x64_20';
        case 'pcg'
          compatibleType = 'pcg64dxsm';
        case 'xoshiro'
          compatibleType = 'xoshiro256pp';
        otherwise
          compatibleType = char(type);
      end
    end
    %=========================================================================
    function name = optionName(value, choices)
      if ~(ischar(value) || (isstring(value) && isscalar(value)))
        error('Nelson:RandStream:invalidNameValueName', 'Invalid name-value argument name.');
      end
      text = lower(char(value));
      matches = choices(startsWith(choices, text));
      if length(matches) ~= 1
        error('Nelson:RandStream:invalidNameValueName', 'Invalid name-value argument name.');
      end
      name = matches{1};
    end
    %=========================================================================
    function value = seedValue(value)
      if ischar(value) || (isstring(value) && isscalar(value))
        if strcmpi(char(value), 'shuffle')
          value = mod(floor(now() * 8640000), 2 ^ 31 - 1);
        else
          error('Nelson:RandStream:invalidSeed', 'Seed must be a nonnegative scalar or shuffle.');
        end
      end
      if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
          && value >= 0 && value < 2 ^ 32)
        error('Nelson:RandStream:seedOutOfRange', 'Seed must be a nonnegative scalar less than 2^32.');
      end
      value = uint32(floor(value));
    end
    %=========================================================================
    function value = positiveInteger(value, name)
      if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
          && value > 0 && value == fix(value))
        error('Nelson:RandStream:aPositiveIntegerScalar2', '%s must be a positive integer scalar.', name);
      end
      value = double(value);
    end
    %=========================================================================
    function value = indexVector(value, name)
      if ~(isnumeric(value) && isreal(value) && isvector(value) && all(isfinite(value(:))) ...
          && all(value(:) > 0) && all(value(:) == fix(value(:))))
        error('Nelson:RandStream:containPositiveIntegerValues2', '%s must contain positive integer values.', name);
      end
      value = double(value(:)).';
    end
    %=========================================================================
    function value = logicalScalar(value, name)
      if islogical(value) && isscalar(value)
        return
      end
      if isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
        value = logical(value);
        return
      end
      error('Nelson:RandStream:trueFalse2', '%s must be true or false.', name);
    end
    %=========================================================================
    function value = normalTransformValue(value)
      if ~(ischar(value) || (isstring(value) && isscalar(value)))
        error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'NormalTransform');
      end
      text = lower(char(value));
      switch text
        case 'ziggurat'
          value = 'Ziggurat';
        case 'polar'
          value = 'Polar';
        case 'inversion'
          value = 'Inversion';
        otherwise
          error('Nelson:RandStream:invalidNormalTransform', 'NormalTransform must be Ziggurat, Polar, or Inversion.');
      end
    end
  end
end
%=============================================================================
