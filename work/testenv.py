import json

# Path to your JSON file
file_path = 'C:\\Users\\ADMIN\\OneDrive\\Documents\\Projects\\Progress\\Simacc\\receipt.json'

# Open and read the JSON file as a text string
with open(file_path, 'r') as file:
    json_text = file.read()

print(json_text)
