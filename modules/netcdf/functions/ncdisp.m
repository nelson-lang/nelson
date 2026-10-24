%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ncdisp(varargin)
  if nargout ~= 0
    error('Nelson:ncdisp:wrongNumberOutputArguments2', 'Wrong number of output arguments.');
  end
  if nargin < 1
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  info = ncinfo(varargin{1});
  disp(['Source: ', info.Filename]);
  if isfield(info, 'Format')
    disp(['Format: ', info.Format]);
  end
  if isfield(info, 'Attributes') && ~isempty(info.Attributes)
    disp('Global attributes:');
    for k = 1:length(info.Attributes)
      disp(['    ', info.Attributes(k).Name]);
    end
  end
  disp('Dimensions:');
  for k = 1:length(info.Dimensions)
    line = ['    ', info.Dimensions(k).Name, ' = ', int2str(info.Dimensions(k).Length)];
    if info.Dimensions(k).Unlimited
      line = [line, ' (unlimited)'];
    end
    disp(line);
  end
  disp('Variables:');
  for k = 1:length(info.Variables)
    line = ['    ', info.Variables(k).Name, '  ', info.Variables(k).Datatype];
    if isfield(info.Variables, 'Size')
      line = [line, '  [', int2str(info.Variables(k).Size), ']'];
    end
    disp(line);
    if isfield(info.Variables, 'Attributes') && ~isempty(info.Variables(k).Attributes)
      for j = 1:length(info.Variables(k).Attributes)
        disp(['        :', info.Variables(k).Attributes(j).Name]);
      end
    end
  end
end
%=============================================================================




