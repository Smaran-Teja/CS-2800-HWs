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
def sameLengthSorted(lon):
    initial_len = len(lon)
    sortNums(lon)
    assert initial_len == len(lon)


def test_addIfNew(lon, n):
    """
    Purpose: Adds an element (n) to a list of numbers, if it doesn't already exist in it
    """
    if n not in lon:
        lon.append(n)
    return lon

@given(st.lists(st.integers()), st.integers())
def addIfNewSize(lon, n):
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

    final_str = "name: " + nsame + "\ninterests: " + interest + "\naddress: " + address

    
    return final_str

@given(student_objects())
def test_json_roundtrip_prop(str):
    si = StudentInfo(str)
    roundtrip = deserialize(serialize(si))
    assert roundtrip.__eq__(si), f"doesn't satisfy roundtrip invariant: {roundtrip}  != {si}"
