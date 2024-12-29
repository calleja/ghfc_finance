import os
from email import policy
from email.parser import BytesParser

def extract_attachments_with_suffix(eml_folder_path: str, output_folder_path: str) -> None:
    """
    Extracts attachments from all .eml files in the specified folder and saves them
    with a sequential numeric suffix in the specified output folder.

    :param eml_folder_path: Path to the folder containing .eml files.
    :param output_folder_path: Path to the folder where attachments will be saved.
    """
    # Ensure the output folder exists
    os.makedirs(output_folder_path, exist_ok=True)

    # Initialize a counter for sequential numeric suffixes
    attachment_counter = 1

    # Loop through all .eml files in the folder
    for filename in os.listdir(eml_folder_path):
        if filename.endswith('.eml'):
            eml_file_path = os.path.join(eml_folder_path, filename)
            print(f"Processing: {eml_file_path}")

            try:
                # Open and parse the .eml file
                with open(eml_file_path, 'rb') as eml_file:
                    msg = BytesParser(policy=policy.default).parse(eml_file)

                # Check for attachments in the email
                for part in msg.iter_attachments():
                    attachment_name = part.get_filename()

                    if attachment_name:  # Ensure the part is an attachment
                        # Extract the file extension
                        file_extension = os.path.splitext(attachment_name)[1]
                        # Create a new filename with a numeric suffix
                        new_filename = f"attachment_{attachment_counter}{file_extension}"
                        output_file_path = os.path.join(output_folder_path, new_filename)

                        # Save the attachment
                        with open(output_file_path, 'wb') as output_file:
                            output_file.write(part.get_payload(decode=True))
                        
                        print(f"Attachment saved as: {output_file_path}")
                        attachment_counter += 1

            except Exception as e:
                print(f"Error processing {eml_file_path}: {e}")

    print("All attachments have been processed.")

if __name__ == "__main__":
    # Prompt user for input folder and output folder
    eml_folder = input("Enter the path to the folder containing .eml files: ").strip()
    output_folder = input("Enter the path to the folder where attachments will be saved: ").strip()

    # Validate the input folder
    if not os.path.exists(eml_folder):
        print(f"Error: The folder {eml_folder} does not exist.")
    else:
        # Extract attachments
        extract_attachments_with_suffix(eml_folder, output_folder)
