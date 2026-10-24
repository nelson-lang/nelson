%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = hgtransform(varargin)
  narginchk(0, 1000);
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);

  if (length(inputArguments) > 0)
    if isgraphics(inputArguments{1})
      if (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') || isgraphics(inputArguments{1}, 'hgtransform'))
        ax = inputArguments{1};
        inputArguments = inputArguments(2:end);
      else
        error('Nelson:hgtransform:parentCanBeOnly', 'Parent can be only an axes or a transform.');
      end
    else
      ax = gca();
    end
  else
    ax = gca();
  end

  inputAsStruct = struct(inputArguments{:});
  if isfield(inputAsStruct, 'Parent')
    ax = inputAsStruct.Parent;
    rmfield(inputAsStruct, 'Parent');
  end
  inputArguments = reshape([fieldnames(inputAsStruct)'; struct2cell(inputAsStruct)'], 1, []);

  h = __hgtransform__('Parent', ax, inputArguments{:});

  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
