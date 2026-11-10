%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) Nh5File < nelson.io.internal.PartialFile
  % Loads and saves parts of the variables of a .nh5 file without reading the whole file.
  methods (Hidden)
    function obj = Nh5File(source, varargin)
      narginchk(1, Inf);
      obj@nelson.io.internal.PartialFile(source, '.nh5', varargin);
    end
  end

  methods (Access = protected)
    function value = readVariable(obj, name, subscripts)
      if iscell(subscripts)
        value = __nh5file_read__(obj.Properties.Source, name, subscripts);
      else
        value = __nh5file_read__(obj.Properties.Source, name);
      end
    end

    function writeVariable(obj, name, value, subscripts)
      if iscell(subscripts)
        __nh5file_write__(obj.Properties.Source, name, value, subscripts);
      else
        __nh5file_write__(obj.Properties.Source, name, value);
      end
    end

    function dims = variableSize(obj, name)
      dims = __nh5file_size__(obj.Properties.Source, name);
    end

    function varargout = fileWho(obj, varargin)
      [varargout{1:nargout}] = whonh5(obj.Properties.Source, varargin{:});
    end

    function varargout = fileWhos(obj, varargin)
      [varargout{1:nargout}] = whosnh5(obj.Properties.Source, varargin{:});
    end
  end
end
%=============================================================================
