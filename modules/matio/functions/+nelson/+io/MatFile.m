%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) MatFile < nelson.io.internal.PartialFile
  % Loads and saves parts of the variables of a MAT-file without reading the whole file.
  methods (Hidden)
    function obj = MatFile(source, varargin)
      narginchk(1, Inf);
      obj@nelson.io.internal.PartialFile(source, '.mat', varargin);
    end
  end

  methods (Access = protected)
    function value = readVariable(obj, name, subscripts)
      if iscell(subscripts)
        value = __matfile_read__(obj.Properties.Source, name, subscripts);
      else
        value = __matfile_read__(obj.Properties.Source, name);
      end
    end

    function writeVariable(obj, name, value, subscripts)
      if iscell(subscripts)
        __matfile_write__(obj.Properties.Source, name, value, subscripts);
      else
        __matfile_write__(obj.Properties.Source, name, value);
      end
    end

    function dims = variableSize(obj, name)
      dims = __matfile_size__(obj.Properties.Source, name);
    end

    function varargout = fileWho(obj, varargin)
      [varargout{1:nargout}] = whomat(obj.Properties.Source, varargin{:});
    end

    function varargout = fileWhos(obj, varargin)
      [varargout{1:nargout}] = whosmat(obj.Properties.Source, varargin{:});
    end
  end
end
%=============================================================================
