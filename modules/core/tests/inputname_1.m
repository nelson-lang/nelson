function R = inputname_1(varargin)
  R = string([]);
  for i = 1:nargin
    R = [R, string(inputname(i))];
  end
end