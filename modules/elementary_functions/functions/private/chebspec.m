%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function CMat = chebspec(nSize, kind, className)
  if isempty(kind)
    kind = 0;
  end
  
  if kind == 1
    nSize = nSize + 1;
  end
  
  n = nSize - 1;
  CMat = zeros(n+1, className);
  
  nodes = cospi((0:n)/n)';
  scale = ones(n+1,1,className); scale(1) = 2; scale(n+1) = 2;
  
  num = scale * ones(1,n+1,className);
  den = nodes - nodes.' + eye(size(CMat), className);
  CMat = (num ./ den);
  
  CMat(1,1) = (2*n^2+1)/6;
  for idx = 2:n+1
    if rem(idx,2) == 0
      CMat(:,idx) = -CMat(:,idx);
      CMat(idx,:) = -CMat(idx,:);
    end
    if idx < n+1
      CMat(idx,idx) = -nodes(idx)/(2*(1-nodes(idx)^2));
    else
      CMat(n+1,n+1) = -CMat(1,1);
    end
  end
  
  if kind == 1
    CMat = CMat(2:n+1,2:n+1);
  end
end
%=============================================================================