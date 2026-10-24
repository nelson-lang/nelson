%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ncc(varargin)
  nargoutchk(0, 1);
  % Module startup scripts can change the current output-count context.
  requestedOutputs = nargout();
  varargout = {};
  __load_compiler__();
  if ~isempty(varargin) && isa(varargin{1}, 'nelson.compiler.BuildOptions')
    if numel(varargin) == 1
      if requestedOutputs == 0
        nelson.compiler.build(varargin{1});
        return;
      end
      result = nelson.compiler.build(varargin{1});
    elseif numel(varargin) == 2 ...
      && (ischar(varargin{2}) || (isstring(varargin{2}) && isscalar(varargin{2}))) ...
      && strcmp(varargin{2}, '--explain-link')
      result = nelson.compiler.analyze(varargin{1});
      if requestedOutputs == 0
        fprintf('%s\n', jsonencode(result));
      end
    else
      error('Nelson:nelsonc:arguments:buildOptionsExplainLinkOnly', 'BuildOptions accepts only the optional --explain-link argument.');
    end
  elseif ~isempty(varargin) && (ischar(varargin{1}) || (isstring(varargin{1}) && isscalar(varargin{1}))) ...
    && strcmp(varargin{1}, 'options')
    result = nelson.compiler.BuildOptions(varargin{2:end});
  else
    result = __nelsonc_command__(varargin, pwd(), requestedOutputs == 0);
  end
  if requestedOutputs > 0
    varargout{1} = result;
  end
end
