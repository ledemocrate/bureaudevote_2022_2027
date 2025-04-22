import praw
import urllib.request
import os

# C:\Users\Quentin GOLLENTZ\AppData\Local\Programs\Python\Python310\Lib\site-packages\praw\praw.ini
# [bot4]
# client_id=3KGYUUD9vTaUI35O3G69EQ
# client_secret=lxdrmytQBrSwZ35jHu-M-eFERwGCaQ
# username=goldentzgrahams
# password=
# user_agent=PyEng Bot 0.1

os.chdir("C:/Users/Quentin GOLLENTZ/Pictures/TEST")
reddit = praw.Reddit('bot4')
praw.Reddit
subreddit = reddit.subreddit("learnpython")
hot_apex = subreddit.top(limit=10)

for item in hot_apex:
  print ('NEW POST--',item.title,'post id --', item) 
  sub_id = reddit.submission(id=item)
  for comment in sub_id.comments:
    print(comment.body)

title = 'Just Made My first Post on Reddit Using Python.'
 
selftext = '''
I am learning how to use the <a href="https://www.jcchouinard.com/reddit-api/">Reddit API with Python</a> using the PRAW wrapper.
By following the tutorial on https://www.jcchouinard.com/post-on-reddit-api-with-python-praw/
This post was uploaded from my Python Script
'''

subreddit.submit(title,selftext=selftext)


######



subreddit = reddit.subreddit("BreastEnvy")
count = 0
# Iterate through top submissions
for submission in subreddit.new(limit=None):

    # Get the link of the submission
    url = str(submission.url)
    name = url.replace("/","")
    # Check if the link is an image
    if url.endswith("jpg") or url.endswith("jpeg") or url.endswith("png"):
        print(url)
        print(name)
        # Retrieve the image and save it in current folder
        urllib.request.urlretrieve(url, f"image{count}" + ".jpg")
        count += 1

        # Stop once you have 10 images
        if count == 10:
            break


