%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ndgrid(varargin)
  % Validate the number of input and output arguments
  validateNumberOfArguments(nargin, nargout);
  
  % If there is only one input and less than two outputs, return the input as a column vector
  if ((nargin == 1) && (nargout < 2))
    varargout{1} = varargin{1}(:);    
    return
  end
  
  % Determine the number of outputs and the size of each dimension
  [numberOfOutputs, indices, sizes] = determineOutputsAndSizes(nargin, nargout, varargin);
  
  % Initialize the output cell array
  varargout = cell(1, max(nargout, 1));
  
  % Generate the grid based on the number of outputs
  if (numberOfOutputs == 2)
    % Special case for 2 outputs
    [varargout{1}, varargout{2}] = generate2DGridCustom(varargin{indices(1)}, varargin{indices(2)});
  else
    % General case for N outputs
    for outputIndex = 1:max(nargout, 1)
      varargout{outputIndex} = generateNDGridCustom(varargin{indices(outputIndex)}, sizes, numberOfOutputs, outputIndex);
    end
  end
end
%=============================================================================
function validateNumberOfArguments(numberOfInputs, numberOfOutputs)
  % Validate the number of input and output arguments
  % Throw an error if there are no inputs or if there are more outputs than inputs
  if (numberOfInputs == 0) || (numberOfInputs > 1 && numberOfOutputs > numberOfInputs)
    error(_('Not enough input arguments.'));
  end
end
%=============================================================================
function [gridX, gridY] = generate2DGridCustom(inputX, inputY)
  % Generate a 2D grid from two input vectors
  inputX = full(inputX(:));  % Convert inputX to a full column vector
  inputY = full(inputY(:)).';  % Convert inputY to a full row vector
  gridX = repmat(inputX, size(inputY));  % Replicate inputX to match the size of inputY
  gridY = repmat(inputY, size(inputX));  % Replicate inputY to match the size of inputX
end
%=============================================================================
function grid = generateNDGridCustom(inputVector, sizes, numberOfOutputs, outputIndex)
  % Generate an N-D grid for the outputIndex-th input vector
  inputVector = full(inputVector);  % Ensure inputVector is a full array
  shapeVector = ones(1, numberOfOutputs);  % Initialize a vector of ones with length numberOfOutputs
  shapeVector(outputIndex) = numel(inputVector);  % Set the outputIndex-th element to the number of elements in inputVector
  inputVector = reshape(inputVector, shapeVector);  % Reshape inputVector according to shapeVector
  replicationSizes = sizes;  % Copy the sizes of the inputs
  replicationSizes(outputIndex) = 1;  % Set the outputIndex-th size to 1
  grid = repmat(inputVector, replicationSizes);  % Replicate inputVector according to replicationSizes
end
%=============================================================================
function [numberOfOutputs, indices, sizes] = determineOutputsAndSizes(numberOfInputs, numberOfOutputs, inputArguments)
  % Determine the number of output arguments and the size of each input
  numberOfOutputs = max(numberOfOutputs, numberOfInputs);  % Maximum of number of outputs or inputs
  if (numberOfInputs == 1)
    if (numberOfOutputs >= 2)
      indices = ones(numberOfOutputs, 1);  % Create a vector of ones for indexing
      sizes(1:numberOfOutputs) = numel(inputArguments{1});  % Size is the number of elements in the single input
    end
  else 
    indices = 1:numberOfOutputs;  % Indices for the inputs
    sizes = cellfun(@numel, inputArguments);  % Get the size of each input
  end
end
%=============================================================================
