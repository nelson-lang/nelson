%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = orderfields(varargin)
  % Check the number of input and output arguments
  narginchk(1, 2);
  nargoutchk(0, 2);
  
  % Initialize the output cell array
  varargout = {};
  
  % Get the first input argument and ensure it is a struct
  structInput = varargin{1};
  mustBeA(structInput, 'struct', 1);
  
  % If there is only one input argument, use orderFieldsOneInput
  if nargin < 2
    [varargout{1:nargout}] = orderFieldsOneInput(structInput, nargout);
    return;
  end
  
  % Get the second input argument
  secondInput = varargin{2};
  
  % Check the type of the second input argument and call the appropriate function
  if iscell(secondInput) || isstring(secondInput)
    [varargout{1:nargout}] = orderFieldsNumericStringOrCell(structInput, secondInput, nargout);
    return;
  end
  
  if isstruct(secondInput)
    [varargout{1:nargout}] = orderFieldsNumericStruct(structInput, secondInput, nargout);
    return;
  end
  
  if isnumeric(secondInput)
    [varargout{1:nargout}] = orderFieldsNumeric(structInput, secondInput, nargout);
    return;
  end
  
  % If the second input argument is of an unexpected type, throw an error
  error(_('Wrong value for #2 arguments: struct, string, cell array of character vectors, or vector expected.'));
end
%=============================================================================
function varargout = orderFieldsOneInput(structInput, numOutputArgs)
  % Sort the field names of the input structure and get the permutation order
  [sortedFieldNames, permutationOrder] = sort(fieldnames(structInput));
  
  % Reorder the structure based on the sorted field names
  reorderedStruct = reorderStruct(structInput, permutationOrder, sortedFieldNames);
  varargout{1} = reorderedStruct;
  
  % If more than one output argument is requested, return the permutation order as well
  if numOutputArgs > 1
    varargout{2} = permutationOrder;
  end 
end
%=============================================================================
function varargout = orderFieldsNumeric(structInput, permutationVector, numOutputArgs)
  % Ensure permutationVector is a column vector
  permutationOrder = permutationVector(:);
  
  % Get the field names of the input structure
  structFieldNames = fieldnames(structInput);
  
  % Check if the length of the permutation vector matches the number of fields
  if (length(permutationOrder) ~= length(structFieldNames))
    error(_('The permutation vector length must match the number of fields in the struct.'));
  end
  
  % Reorder the field names according to the permutation vector
  reorderedFieldNames = structFieldNames(permutationOrder);
  
  % Reorder the structure based on the computed permutation order
  reorderedStruct = reorderStruct(structInput, permutationOrder, reorderedFieldNames);
  varargout{1} = reorderedStruct;
  
  % If more than one output argument is requested, return the permutation order as well
  if numOutputArgs > 1
    varargout{2} = permutationOrder;
  end 
end
%=============================================================================
function varargout = orderFieldsNumericStruct(structInput1, structInput2, numOutputArgs)
  % Sort the field names of the first structure and get the permutation order
  [sortedFieldNames1, permutationOrder1] = sort(fieldnames(structInput1));
  
  % Get and sort the field names of the second structure
  fieldNamesStruct2 = fieldnames(structInput2);
  [sortedFieldNames2, permutationOrder2] = sort(fieldNamesStruct2);
  
  % Ensure both structures have exactly matching fields
  if ~all(strcmp(sortedFieldNames1, sortedFieldNames2))
    error(_('Both structures must have exactly matching fields.'));
  end
  
  % Find the inverse permutation order for the second structure
  inversePermutationOrder2(permutationOrder2) = (1:length(permutationOrder2))';
  
  % Apply the permutation order to reorder the fields of the first structure
  permutationOrder = permutationOrder1(inversePermutationOrder2);
  
  % Reorder the first structure based on the computed permutation order
  reorderedStruct = reorderStruct(structInput1, permutationOrder, fieldNamesStruct2);
  
  % Set the output arguments
  varargout{1} = reorderedStruct;
  
  % If more than one output argument is requested, return the permutation order as well
  if numOutputArgs > 1
    varargout{2} = permutationOrder;
  end 
end
%=============================================================================
function varargout = orderFieldsNumericStringOrCell(structInput, textInput, numOutputArgs)
  % Sort the field names of the structure and get the permutation order
  [sortedFieldNamesStruct, permutationOrderStruct] = sort(fieldnames(structInput));
  
  % Ensure textInput is a column vector and sort it
  [sortedTextInput, permutationOrderTextInput] = sort(textInput(:));
  
  % Ensure the text in the second input matches the field names of the structure
  if ~all(strcmp(sortedFieldNamesStruct, sortedTextInput))
    error(_('The text in the second input needs to correspond with the field names of the struct.'));
  end
  
  % Find the inverse permutation order for the text input
  inversePermutationOrderTextInput(permutationOrderTextInput) = (1:length(permutationOrderTextInput))';
  
  % Apply the permutation order to reorder the fields of the structure
  permutationOrder = permutationOrderStruct(inversePermutationOrderTextInput);
  newFields = textInput;
  
  % Reorder the structure based on the computed permutation order
  reorderedStruct = reorderStruct(structInput, permutationOrder, newFields);
  
  % Set the output arguments
  varargout{1} = reorderedStruct;
  
  % If more than one output argument is requested, return the permutation order as well
  if numOutputArgs > 1
    varargout{2} = permutationOrder;
  end 
end
%=============================================================================
function reorderedStruct = reorderStruct(inputStruct, permutationOrder, newFieldNames)
  % Get the original size of the input structure
  originalSize = size(inputStruct);
  
  % Convert the structure to a cell array
  allValuesCellArray = struct2cell(inputStruct(:));
  
  % Reorder the cell array according to the permutation order
  allValuesCellArray(:, :) = allValuesCellArray(permutationOrder, :);
  
  % Convert the reordered cell array back to a structure with new field names
  reorderedStruct = cell2struct(allValuesCellArray, cellstr(newFieldNames), 1);
  
  % Reshape the reordered structure to match the original size
  reorderedStruct = reshape(reorderedStruct, originalSize);
end
%=============================================================================
