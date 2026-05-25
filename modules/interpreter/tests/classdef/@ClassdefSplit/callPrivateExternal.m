function r = callPrivateExternal(obj)
  r = privateExternal(obj) + obj.privateExternal();
end
