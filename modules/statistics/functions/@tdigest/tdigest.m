%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef tdigest
  properties (Dependent)
    compression
    totalWeight
  end

  properties (Hidden)
    compressionValue = 100
    totalWeightValue = 0
    means = []
    weights = []
    version = 2.0
  end

  methods
    function obj = tdigest(varargin)
      x = [];
      compression = 100;
      hasData = false;
      narginchk(0, 2);

      if nargin == 1
        arg = varargin{1};
        if isscalar(arg) && isnumeric(arg)
          compression = arg;
        elseif isnumeric(arg)
          x = arg;
          hasData = true;
        else
          error('Nelson:statistics:invalidInputArgumentExpectedNumeric', 'Invalid input argument. Expected a numeric array or a scalar compression value.');
        end
      elseif nargin == 2
        compression = varargin{1};
        x = varargin{2};
        hasData = true;
        tdigest.validateData(x);
      end

      obj.compression = compression;
      obj.totalWeightValue = 0;
      obj.means = [];
      obj.weights = [];
      obj.version = 2.0;

      if hasData
        tdigest.validateData(x);
        obj = obj.addData(x);
      end
    end

    function obj = set.compression(obj, value)
      tdigest.validateCompression(value);
      obj.compressionValue = value;
    end

    function value = get.compression(obj)
      value = obj.compressionValue;
    end

    function obj = set.totalWeight(obj, value)
      if ~isscalar(value) || ~isnumeric(value) || ~isreal(value) || ~isfinite(value) || value < 0
        error('Nelson:statistics:totalWeightNonNegativeScalar', 'Total weight must be a nonnegative scalar number.');
      end
      obj.totalWeightValue = value;
    end

    function value = get.totalWeight(obj)
      value = obj.totalWeightValue;
    end

    function qval = quantile(obj, q)
      if ~isvector(q)
        error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Input quantiles');
      end
      if any(q(:) < 0)
        error('Nelson:statistics:valueLeast0', 'Value must be at least 0.');
      end
      if any(q(:) > 1)
        error('Nelson:statistics:valueLessEqual1', 'Value must be less than or equal to %d.', 1);
      end
      q = q(:);

      if isempty(obj.means)
        qval = NaN(size(q));
        return
      end

      [sortedMeans, idx] = sort(obj.means);
      sortedWeights = obj.weights(idx);

      if length(sortedMeans) == 1
        qval = repmat(sortedMeans(1), size(q));
        return
      end

      cumw = cumsum(sortedWeights);
      centroidPositions = (cumw - sortedWeights / 2) / obj.totalWeight;
      quantilePositions = [0; centroidPositions; 1];
      interpValues = [sortedMeans(1); sortedMeans; sortedMeans(end)];
      qval = interp1(quantilePositions, interpValues, q, 'linear');
      qval = qval(:);
    end

    function qval = percentile(obj, p)
      if any(p(:) < 0)
        error('Nelson:statistics:valueLeast0', 'Value must be at least 0.');
      end
      if any(p(:) > 100)
        error('Nelson:statistics:valueLessEqual100', 'Value must be less than or equal to %d.', 100);
      end
      qval = obj.quantile(p / 100);
    end

    function obj = plus(a, b)
      if isa(a, 'tdigest') && isnumeric(b)
        obj = a.addData(b);
        return
      end
      if isa(b, 'tdigest') && isnumeric(a)
        obj = b.addData(a);
        return
      end
      error('Nelson:statistics:combiningTdigestObjectsAllowedThey', 'Combining ''tdigest'' objects is not allowed as they are designed to be scalar.');
    end

    function obj = compress(obj)
      if isempty(obj.totalWeight) || obj.totalWeight == 0
        return
      end
      if isempty(obj.means) || numel(obj.means) <= 1
        return
      end

      [means, idx] = sort(obj.means);
      weights = obj.weights(idx);

      n = numel(means);
      mergedMeans = zeros(n, 1);
      mergedWeights = zeros(n, 1);
      idxMerged = 1;
      curMean = means(1);
      curWeight = weights(1);
      cumulative = 0;
      totalWeightFactor = 4 * obj.totalWeight / obj.compression;
      invTotalWeight = 1 / obj.totalWeight;

      for k = 2:n
        q = cumulative * invTotalWeight;
        maxWeight = totalWeightFactor * q * (1 - q);
        if curWeight + weights(k) <= maxWeight
          newWeight = curWeight + weights(k);
          curMean = curMean + (means(k) - curMean) * weights(k) / newWeight;
          curWeight = newWeight;
        else
          mergedMeans(idxMerged) = curMean;
          mergedWeights(idxMerged) = curWeight;
          idxMerged = idxMerged + 1;
          curMean = means(k);
          curWeight = weights(k);
        end
        cumulative = cumulative + weights(k);
      end

      mergedMeans(idxMerged) = curMean;
      mergedWeights(idxMerged) = curWeight;
      mergedMeans = mergedMeans(1:idxMerged);
      mergedWeights = mergedWeights(1:idxMerged);

      maxCentroids = ceil(obj.compression);
      while numel(mergedMeans) > maxCentroids && numel(mergedMeans) > 1
        adjSums = mergedWeights(1:end - 1) + mergedWeights(2:end);
        [~, minIdx] = min(adjSums);
        newWeight = mergedWeights(minIdx) + mergedWeights(minIdx + 1);
        newMean = (mergedMeans(minIdx) * mergedWeights(minIdx) ...
          + mergedMeans(minIdx + 1) * mergedWeights(minIdx + 1)) / newWeight;
        mergedMeans(minIdx) = newMean;
        mergedWeights(minIdx) = newWeight;
        mergedMeans(minIdx + 1) = [];
        mergedWeights(minIdx + 1) = [];
      end

      obj.means = mergedMeans;
      obj.weights = mergedWeights;
    end

    function disp(obj)
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
      fprintf('  tdigest object with compression factor: %d\n', obj.compression);
      fprintf('  Total weight: %d\n', obj.totalWeight);
      fprintf('  Number of centroids: %d\n', length(obj.means));
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
      if ~isempty(name)
        fprintf('%s = \n', name);
      end
      disp(obj);
    end

    function n = numel(varargin)
      n = 1;
    end

    function obj = horzcat(varargin)
      error('Nelson:statistics:combiningTdigestObjectsAllowedThey', 'Combining ''tdigest'' objects is not allowed as they are designed to be scalar.');
    end

    function obj = vertcat(varargin)
      error('Nelson:statistics:combiningTdigestObjectsAllowedThey', 'Combining ''tdigest'' objects is not allowed as they are designed to be scalar.');
    end
  end

  methods (Access = private)
    function obj = addData(obj, x)
      tdigest.validateData(x);
      valid = x(~isnan(x));
      if isempty(valid)
        return
      end

      needsCompression = obj.shouldCompress();
      obj.means = [obj.means; valid(:)];
      obj.weights = [obj.weights; ones(numel(valid), 1)];
      obj.totalWeight = obj.totalWeight + numel(valid);

      if needsCompression
        obj = obj.compress();
      end
    end

    function tf = shouldCompress(obj)
      threshold = max(obj.compression * 1.5, obj.compression + 50);
      tf = numel(obj.means) > threshold;
    end
  end

  methods (Static, Access = private)
    function validateCompression(value)
      if ~isscalar(value) || ~isnumeric(value) || ~isreal(value) || ~isfinite(value) || value <= 0
        error('Nelson:statistics:compressionParameterPositiveScalarNumber', 'Compression parameter must be a positive scalar number.');
      end
    end

    function validateData(value)
      if ~isnumeric(value) || ~isreal(value)
        error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Data');
      end
    end
  end
end
%=============================================================================
