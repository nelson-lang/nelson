%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef netcdf
  methods (Static)
    function varargout = abort(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.abort', varargin{:});
    end
    function varargout = close(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.close', varargin{:});
    end
    function varargout = copyAtt(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.copyAtt', varargin{:});
    end
    function varargout = create(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.create', varargin{:});
    end
    function varargout = defDim(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defDim', varargin{:});
    end
    function varargout = defGrp(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defGrp', varargin{:});
    end
    function varargout = defVar(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defVar', varargin{:});
    end
    function varargout = defVarChunking(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defVarChunking', varargin{:});
    end
    function varargout = defVarDeflate(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defVarDeflate', varargin{:});
    end
    function varargout = defVarFill(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defVarFill', varargin{:});
    end
    function varargout = defVarFletcher32(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defVarFletcher32', varargin{:});
    end
    function varargout = defVlen(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.defVlen', varargin{:});
    end
    function varargout = delAtt(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.delAtt', varargin{:});
    end
    function varargout = endDef(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.endDef', varargin{:});
    end
    function varargout = getAtt(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.getAtt', varargin{:});
    end
    function varargout = getChunkCache(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.getChunkCache', varargin{:});
    end
    function varargout = getConstant(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.getConstant', varargin{:});
    end
    function varargout = getConstantNames(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.getConstantNames', varargin{:});
    end
    function varargout = getVar(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.getVar', varargin{:});
    end
    function varargout = inq(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inq', varargin{:});
    end
    function varargout = inqAtt(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqAtt', varargin{:});
    end
    function varargout = inqAttID(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqAttID', varargin{:});
    end
    function varargout = inqAttName(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqAttName', varargin{:});
    end
    function varargout = inqDim(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqDim', varargin{:});
    end
    function varargout = inqDimID(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqDimID', varargin{:});
    end
    function varargout = inqDimIDs(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqDimIDs', varargin{:});
    end
    function varargout = inqFormat(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqFormat', varargin{:});
    end
    function varargout = inqGrpName(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqGrpName', varargin{:});
    end
    function varargout = inqGrpNameFull(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqGrpNameFull', varargin{:});
    end
    function varargout = inqGrpParent(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqGrpParent', varargin{:});
    end
    function varargout = inqGrps(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqGrps', varargin{:});
    end
    function varargout = inqLibVers(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqLibVers', varargin{:});
    end
    function varargout = inqNcid(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqNcid', varargin{:});
    end
    function varargout = inqUnlimDims(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqUnlimDims', varargin{:});
    end
    function varargout = inqUserType(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqUserType', varargin{:});
    end
    function varargout = inqVar(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVar', varargin{:});
    end
    function varargout = inqVarChunking(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVarChunking', varargin{:});
    end
    function varargout = inqVarDeflate(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVarDeflate', varargin{:});
    end
    function varargout = inqVarFill(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVarFill', varargin{:});
    end
    function varargout = inqVarFletcher32(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVarFletcher32', varargin{:});
    end
    function varargout = inqVarID(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVarID', varargin{:});
    end
    function varargout = inqVarIDs(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVarIDs', varargin{:});
    end
    function varargout = inqVlen(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.inqVlen', varargin{:});
    end
    function varargout = open(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.open', varargin{:});
    end
    function varargout = putAtt(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.putAtt', varargin{:});
    end
    function varargout = putVar(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.putVar', varargin{:});
    end
    function varargout = reDef(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.reDef', varargin{:});
    end
    function varargout = renameAtt(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.renameAtt', varargin{:});
    end
    function varargout = renameDim(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.renameDim', varargin{:});
    end
    function varargout = renameVar(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.renameVar', varargin{:});
    end
    function varargout = setChunkCache(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.setChunkCache', varargin{:});
    end
    function varargout = setDefaultFormat(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.setDefaultFormat', varargin{:});
    end
    function varargout = setFill(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.setFill', varargin{:});
    end
    function varargout = sync(varargin)
      [varargout{1:nargout}] = __netcdf__('netcdf.sync', varargin{:});
    end
  end
end
%=============================================================================