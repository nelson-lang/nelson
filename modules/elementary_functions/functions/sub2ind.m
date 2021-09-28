%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
