package metascala.features.controlflow;

public class Switches {
    public static int smallSwitch(int a){
        switch(a){
            case 0:  return 1;
            case 1:  return 0;
            default: return 2;
        }
    }
    public static double bigDenseSwitch(int a){
        switch(a){
            case 0:  return 1123.213;
            case 1:  return 3212.321;
            case 2:  return 123123.123;
            case 3:  return 123.312;
            case 4:  return 123123.1231;
            case 5:  return 1231.3212;
            case 6:  return 123.132123;
            case 7:  return 32123.123;
            case 8:  return 123123.12312;
            case 9:  return 123123.3123;
            case 10: return 123123.1312;
            case 11: return 123123.2123;
            case 12: return 123321.123;
            case 13: return 123123.12312;
            case 14: return 123123.1231;
            case 15: return 1321231.1231;
            case 16: return 23123123.1231;
            case 17: return 123123123.123123;
            case 18: return 123123.1231;
            case 19: return 23123.12321;
            case 20: return 12312312.321;
            case 21: return 1232312.312;
            case 22: return 123123123.132123;
            case 23: return 123123123.1231;
            case 24: return 132123.1231;
            case 25: return 12312321.123;
            case 26: return 1232123.312;
            case 27: return 123123.12312;
            case 28: return 13212312.123123;
            case 29: return 2123123.1231231;
            default: return 123123.123123;
        }
    }
    public static double bigSparseSwitch(int a){
        switch(a){
            case 1:         return 3212.321;
            case 2:         return 123123.123;
            case 4:         return 123.312;
            case 8:         return 123123.1231;
            case 16:        return 1231.3212;
            case 32:        return 123.132123;
            case 62:        return 32123.123;
            case 128:       return 123123.12312;
            case 256:       return 123123.3123;
            case 512:       return 123123.1312;
            case 1024:      return 123123.2123;
            case 2048:      return 123321.123;
            case 4096:      return 123123.12312;
            case 8192:      return 123123.1231;
            case 16384:     return 1321231.1231;
            case 32768:     return 23123123.1231;
            case 65536:     return 123123123.123123;
            case 131072:    return 123123.1231;
            case 262144:    return 23123.12321;
            case 524288:    return 12312312.321;
            case 1048576:   return 1232312.312;
            case 2097152:   return 123123123.132123;
            default: return 123123.123123;
        }
    }
    public static int charSwitch(char c){
        switch(c){
            case 'a': return 1;
            case 'b': return 2;
            case 'c': return 3;
            case 'd': return 4;
            case 'e': return 5;
            case 'f': return 6;
            case 'g': return 7;
            case 'h': return 8;
            case 'i': return 9;
            case 'j': return 0;
            default: return 10;
        }
    }
    public static int byteSwitch(byte b){
        switch(b){
            case 1:     return 1;
            case 2:     return 2;
            case 4:     return 3;
            case 16:    return 4;
            case 32:    return 5;
            case 64:    return 6;
            case 127:   return 7;
            case -128:  return 8;
            default:    return 10;
        }
    }
    public static int stringSwitch(int n){
        switch("" + n){
            case "0": return 0;
            case "1": return 1;
            default:  return 2;
        }
    }
    public static String stringSwitchTwo(String s){
        switch(s){
            case "omg": return "iam";
            case "wtf": return "cow";
            case "bbq": return "hearme";
            default:    return "moo";
        }
    }
}
