from  hypothesis import given, strategies as st
import json
import random
import pytest
def sortNums(lon):
    """
    Purpose: given a list of numbers, sorts the list
    """
    lon.sort()

@given(st.lists(st.integers()))
def test_sameLengthSorted(lon):
    initial_len = len(lon)
    sortNums(lon)
    assert initial_len == len(lon)



def addIfNew(lon, n):
    """
    Purpose: Adds an element (n) to a list of numbers, if it doesn't already exist in it
    """
    if n not in lon:
        lon.append(n)
    return lon

@given(st.lists(st.integers()), st.integers())
def test_addIfNewSize(lon, n):
    assert len(lon) <= len(addIfNew(lon, n))





class StudentInfo: 
    """ 
    Purpose: class that contains a constructor, which accepts a str containing name, address, interests
    and constructs an object with given name, list of interests, and address
    """
    def __init__(self, info_string):
        data = info_string.split("\n")
        self.name = data[0].split(": ")[1]
        self.interests = data[1].split(": ")[1].split(", ")
        self.address = data[2].split(": ")[1]

    def __eq__(self, other): 
        if not isinstance(other, StudentInfo):
            return NotImplemented
        return self.name == other.name and self.interests == other.interests and self.address == other.address



def serialize(si):
    """Purpose: given a StudentInfo, converts the data into JSON format"""
    return json.dumps({
        'name' : si.name,
        'interests' : si.interests,
        'address' : si.address,
    })

def deserialize(json_str):
    """Purpose: given json information, converts into a StudentInfo object"""
    data = json.loads(json_str)
    name = "name: " + data['name']
    interests = "interests: " + ', '.join(data['interests'])
    address = "address: " + data['address']
    info_string = '\n'.join([name, interests, address])
    return StudentInfo(info_string)





filtered_alphabet = st.characters(whitelist_categories=('Lu', 'Ll', 'Nd', 'Pc', 'Pd', 'Ps', 'Pe', 'Pi', 'Pf', 'Po'),   
                                            blacklist_characters=',\n')
# Define a strategy for generating random names
names = st.text(min_size=1, max_size=50, alphabet = filtered_alphabet)

# Define a strategy for generating a random interest
interests = st.text(min_size=0, max_size=15, 
                    alphabet = filtered_alphabet)

# Define a strategy to generate random addresses
addresses = st.text(min_size=0, max_size=30, alphabet = filtered_alphabet)

# Define a custom strategy for generating random Person objects
@st.composite
def student_objects(draw):
    name = draw(names)

    interest = ""
    randNum = random.randint(1, 5)

    for i in range (0, randNum):
        interest += draw(interests)
        if (not i == randNum - 1):
            interest += ", "
    
    address = draw(addresses)

    final_str = "name: " + name + "\ninterests: " + interest + "\naddress: " + address

    
    return final_str

@given(student_objects())
def test_json_roundtrip_prop(str):
    si = StudentInfo(str)
    roundtrip = deserialize(serialize(si))
    assert roundtrip.__eq__(si), f"doesn't satisfy roundtrip invariant: {roundtrip}  != {si}"



class Color:

    def __init__(self, color = "", r = -1, g = -1, b = -1, c = -1, m = -1, y = -1, k = -1):
        self.color = color
        self.r = r
        self.g = g
        self.b = b
        self.c = c
        self.m = m
        self.y = y
        self.k = k

        if (color != ""):
            self.rep = 0

        elif (r >= 0 and g >= 0 and b >= 0):
            self.rep = 1
            
        elif (c >= 0 and m >= 0 and y >= 0 and k >= 0):
            self.rep = 2
        
        else: 
            raise Exception("No discernable representation provided")

    
    def __eq__(self, other): 
        if not isinstance(other, Color):
            return NotImplemented
        
        if(self.rep == 0 and other.rep == 0):
            return self.color == other.color
        elif(self.rep == 1 and other.rep == 1):
            return self.r == other.r and self.g == other.g and self.b == other.b
        elif (self.rep == 2 and other.rep == 2):
            return self.c == other.c and self.m == other.m and self.y == other.y and self.k == other.k
        else:
            return False

    

    
    



def pack(color):
    colorList = []
    colorList.append(color.rep)
    colorList.append(color.color)
    colorList.append(color.r)
    colorList.append(color.g)
    colorList.append(color.b)
    colorList.append(color.c)
    colorList.append(color.m)
    colorList.append(color.y)
    colorList.append(color.k)

    return colorList

def unpack(lon):
    # case where color is a string color
    if (lon[0] == 0):
        return Color(color = lon[1])
    
    # case where color is rgb
    elif (lon[0] == 1):
        if (lon[2] < 0 or lon[3] < 0 or lon[4] < 0):
            raise Exception("tried to unpack rgb, but given negative values")
        return Color(r = lon[2], g = lon[3], b = lon[4])

    # case where color is cmyk
    else :
        if (lon[5] < 0 or lon[6] < 0 or lon[7] < 0 or lon[8] < 0):
            raise Exception("tried to unpack cmyk, but given negative values")
        return Color(c = lon[5], m = lon[6], y = lon[7], k = lon[8])



@given(st.builds(Color, r = st.integers(min_value=0, max_value=255), g = st.integers(min_value=0, max_value=255), b = st.integers(min_value=0, max_value=255)))
def test_roundtrip_pack_unpack_rgb(color):
    assert unpack(pack(color)).__eq__(color)


@given(st.builds(Color, color = st.text(min_size=1, max_size=5)))
def test_roundtrip_pack_unpack_color(color):
    assert unpack(pack(color)).__eq__(color)


@given(st.builds(Color, c = st.integers(min_value=0, max_value=255), 
                        m = st.integers(min_value=0, max_value=255), 
                        y = st.integers(min_value=0, max_value=255), 
                        k = st.integers(min_value=0, max_value=255)))

def test_roundtrip_pack_unpack_cmyk(color):
    assert unpack(pack(color)).__eq__(color)


