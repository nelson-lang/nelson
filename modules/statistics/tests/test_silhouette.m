%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('silhouette'), 3);
asserts.isequal(nargout('silhouette'), 2);
%=============================================================================
X = [0; 1; 10; 11];
clust = [1; 1; 2; 2];
s = silhouette(X, clust);
asserts.isequal(size(s), [4 1]);
asserts.istrue(all(s > 0.8));
[s, h] = silhouette(X, clust, 'cityblock');
asserts.isequal(size(s), [4 1]);
asserts.isequal(h, []);
%=============================================================================
s = silhouette([0; 10; 20], [1; 2; 3]);
asserts.isequal(s, zeros(3, 1));
%=============================================================================
asserts.checkerror('silhouette()', _('Not enough input arguments.'));
asserts.checkerror('silhouette([1; 2], [1])', _('CLUST must have one value per row of X.'));
asserts.checkerror('silhouette([1; 2], [1; 2], ''bad'')', sprintf(_('Invalid %s value.'), 'Distance'));
%=============================================================================
