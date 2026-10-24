%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isMATLABReleaseOlderThan(release, varargin)
  % Release gate helper.
  % Nelson reports a recent release (see verLessThan) so that gates against
  % older releases resolve to false and toolboxes take their current code paths.
  % Nelson is treated as
  % carrying every update of that release, so update-level checks against the
  % emulated release also resolve to false.
  narginchk(1, 3);
  nargoutchk(0, 1);
  if ~(ischar(release) || (isstring(release) && isscalar(release)))
    error('Nelson:types:wrongArgumentTypeExpected', _('Wrong type for argument #%d: %s expected.'), 1, 'string');
  end
  [givenYear, givenHalf] = parseRelease(char(release));
  givenUpdate = 0;
  if ~isempty(varargin)
    if numel(varargin) ~= 2 || ~(ischar(varargin{1}) || (isstring(varargin{1}) && isscalar(varargin{1}))) || ~strcmpi(char(varargin{1}), 'update')
      error('Nelson:isMATLABReleaseOlderThan:invalidOption', _('The option must be ''update'' followed by an integer value.'));
    end
    givenUpdate = varargin{2};
    if ~(isnumeric(givenUpdate) && isscalar(givenUpdate) && isreal(givenUpdate) && givenUpdate == fix(givenUpdate) && givenUpdate >= 0)
      error('Nelson:isMATLABReleaseOlderThan:invalidUpdate', _('The update value must be a nonnegative integer.'));
    end
  end
  % Emulated current release (see matlabCompatibleVersion in verLessThan).
  currentYear = 2025;
  currentHalf = 1; % 'a'
  currentUpdate = Inf;
  current = [currentYear, currentHalf, currentUpdate];
  given = [givenYear, givenHalf, double(givenUpdate)];
  tf = logical(isLexLess(current, given));
end
%=============================================================================
function [releaseYear, releaseHalf] = parseRelease(release)
  tokens = regexp(release, '^[Rr](\d{4})([ab])$', 'tokens', 'once');
  if isempty(tokens)
    error('Nelson:isMATLABReleaseOlderThan:invalidRelease', _('The release must be of the form ''RYYYYa'' or ''RYYYYb'', such as ''R2023b''.'));
  end
  releaseYear = str2double(tokens{1});
  if strcmpi(tokens{2}, 'a')
    releaseHalf = 1;
  else
    releaseHalf = 2;
  end
end
%=============================================================================
function tf = isLexLess(a, b)
  tf = false;
  for k = 1:numel(a)
    if a(k) < b(k)
      tf = true;
      return;
    elseif a(k) > b(k)
      return;
    end
  end
end
%=============================================================================
