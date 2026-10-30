%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isKey(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  obj = varargin{1};
  key = varargin{2};
  % All inline at the method level (direct property reads, no types()/keyCell
  % helper dispatch) so the cost is one method call plus the native membership.
  kt = obj.keyType;
  if isempty(kt)
    error('Nelson:dictionary:unablePerformADictionaryLookup3', 'Unable to perform a dictionary lookup when the key and value types are not set.');
  end
  if strcmp(kt, 'string') && ischar(key)
    key = string(key);
  end
  nK = numel(key);
  kc = cell(1, nK);
  for k = 1:nK
    kk = key(k);
    if ~isa(kk, kt)
      kk = convertDataType(kt, kk);
    end
    kc{k} = kk;
  end
  varargout{1} = reshape(__dictionary_store__(obj.store, 'iskey', kc), size(key));
end
%=============================================================================
