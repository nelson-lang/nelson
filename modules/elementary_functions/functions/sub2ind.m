%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ind = sub2ind(szvec, varargin)
  ind = [];
  if (length(varargin) ~= 0)
    szvec = szvec(:)';
    if (length(szvec) < length(varargin))
      szvec = [szvec, ones(1, length(varargin) - length(szvec))];
    end
    indvecs = {};
    nlength = length(varargin{1}(:));
    for i=1:length(varargin)
      indvecs{i} = varargin{i}(:);
      if (length(indvecs{i}) ~= nlength)
        error(_('Indexing arguments must have the same length.'));
      end
      if (min(indvecs{i}) < 1)
        error(_('Indexing arguments are out of range of an array of the given size.'));
      end
    end
    out = zeros(nlength, 1);
    szslice = 1;
    for i = 1:length(varargin)
      out = out + szslice * (indvecs{i} - 1);
      szslice = szslice * szvec(i);
    end
    ind = (out + 1)';
    ind = reshape(ind, size(varargin{1}));
  end
end
%=============================================================================
