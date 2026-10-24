%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.checkerror('imregconfig(1)', sprintf(_('%s must be a string.'), 'Registration modality'));
%=============================================================================
asserts.checkerror('imregconfig(''unknown'')', sprintf(_('Unsupported "%s" option.'), 'registration modality'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1; 2], ''translation'', OPT_ERR, MET_ERR)', ...
  sprintf(_('%s must have the same size.'), 'Moving and fixed images'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], 1, OPT_ERR, MET_ERR)', ...
  sprintf(_('%s must be a string.'), 'Registration transformation type'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''projective'', OPT_ERR, MET_ERR)', ...
  sprintf(_('Unsupported "%s" option.'), 'registration transformation type'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', 1, MET_ERR)', ...
  _('Registration optimizer must be a structure.'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
OPT_BAD = OPT_ERR;
OPT_BAD.MaximumIterations = 0;
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_BAD, MET_ERR)', ...
  sprintf(_('%s must be a positive integer scalar.'), 'MaximumIterations'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
OPT_BAD = OPT_ERR;
OPT_BAD.AngleSearch = Inf;
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_BAD, MET_ERR)', ...
  sprintf(_('%s must be a nonnegative finite scalar.'), 'AngleSearch'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
OPT_BAD = OPT_ERR;
OPT_BAD.ScaleSearch = 1;
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_BAD, MET_ERR)', ...
  sprintf(_('%s must be a finite scalar in the range [%s].'), 'ScaleSearch', '0, 1'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
OPT_BAD = OPT_ERR;
OPT_BAD.ShearSearch = 1;
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_BAD, MET_ERR)', ...
  sprintf(_('%s must be a finite scalar in the range [%s].'), 'ShearSearch', '0, 1'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, struct())', ...
  _('Registration metric must contain a Type field.'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, 1)', ...
  sprintf(_('%s must be a string.'), 'Registration metric type'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, struct(''Type'', ''bad''))', ...
  sprintf(_('Unsupported "%s" option.'), 'registration metric'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, 1, 2)', ...
  sprintf(_('%s option name must be a string.'), 'Registration'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''Unknown'', true)', ...
  sprintf(_('Unsupported "%s" option.'), 'registration'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''Interpolation'', 1)', ...
  sprintf(_('%s must be a string.'), 'Interpolation method'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''Interpolation'', ''bad'')', ...
  sprintf(_('Unsupported "%s" option.'), 'interpolation method'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregtform([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''PyramidLevels'', 0)', ...
  sprintf(_('%s must be a positive integer scalar.'), 'PyramidLevels'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregister([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''Unknown'', true)', ...
  sprintf(_('Unsupported "%s" option.'), 'registration'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregister([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, 1, 2)', ...
  sprintf(_('%s option name must be a string.'), 'Registration'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregister([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''Interpolation'', 1)', ...
  sprintf(_('%s must be a string.'), 'Interpolation method'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregister([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''Interpolation'', ''bad'')', ...
  sprintf(_('Unsupported "%s" option.'), 'interpolation method'));
%=============================================================================
[OPT_ERR, MET_ERR] = imregconfig('monomodal');
asserts.checkerror('imregister([1 2], [1 2], ''translation'', OPT_ERR, MET_ERR, ''PyramidLevels'', 0)', ...
  sprintf(_('%s must be a positive integer scalar.'), 'PyramidLevels'));
