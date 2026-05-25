classdef ClassdefSplitDerived < ClassdefSplit
  methods
    function r = callProtectedExternal(obj)
      r = protectedExternal(obj) + obj.protectedExternal();
    end
  end
end
