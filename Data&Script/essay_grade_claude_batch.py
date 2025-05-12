import os
import json
import anthropic
from anthropic.types.message_create_params import MessageCreateParamsNonStreaming
from anthropic.types.messages.batch_create_params import Request
from itertools import chain

# Set your Anthropic API key
client = anthropic.Anthropic(api_key="API KEY goes here")

# Path to the folder containing essays
essay_folder = os.path.expanduser("texts")

# Read all essays
essays = {}
for filename in os.listdir(essay_folder):
    if filename.endswith(".txt"):
        with open(os.path.join(essay_folder, filename), "r", encoding="utf-8") as file:
            essays[filename] = file.read()

# Define the prompt
def create_prompt(essay):
    rubric = """ SCORE OF 6: An essay in this category demonstrates
    clear and consistent mastery, although it may have a few minor
    errors. A typical essay effectively and insightfully develops a
    point of view on the issue and demonstrates outstanding critical
    thinking, using clearly appropriate examples, reasons, and other
    evidence to support its position; is well organized and clearly
    focused, demonstrating clear coherence and smooth progression of
    ideas; exhibits skillful use of language, using a varied, accurate,
    and apt vocabulary; demonstrates meaningful variety in sentence
    structure; is free of most errors in grammar, usage, and mechanics.

    SCORE OF 5: An essay in this category demonstrates reasonably
    consistent mastery, although it will have occasional errors or
    lapses in quality. A typical essay effectively develops a point of
    view on the issue and demonstrates strong critical thinking,
    generally using appropriate examples, reasons, and other evidence to
    support its position; is well organized and focused, demonstrating
    coherence and progression of ideas; exhibits facility in the use of
    language, using appropriate vocabulary; demonstrates variety in
    sentence structure; is generally free of most errors in grammar,
    usage, and mechanics.

    SCORE OF 4: An essay in this category demonstrates adequate mastery,
    although it will have lapses in quality. A typical essay develops a
    point of view on the issue and demonstrates competent critical
    thinking, using adequate examples, reasons, and other evidence to
    support its position; is generally organized and focused,
    demonstrating some coherence and progression of ideas; exhibits
    adequate but inconsistent facility in the use of language, using
    generally appropriate vocabulary; demonstrates some variety in
    sentence structure; has some errors in grammar, usage, and
    mechanics.

    SCORE OF 3: An essay in this category demonstrates developing
    mastery, and is marked by ONE OR MORE of the following weaknesses:
    develops a point of view on the issue, demonstrating some critical
    thinking, but may do so inconsistently or use inadequate examples,
    reasons, or other evidence to support its position; is limited in
    its organization or focus, or may demonstrate some lapses in
    coherence or progression of ideas; displays developing facility in
    the use of language, but sometimes uses weak vocabulary or
    inappropriate word choice; lacks variety or demonstrates problems in
    sentence structure; contains an accumulation of errors in grammar,
    usage, and mechanics.

    SCORE OF 2: An essay in this category demonstrates little mastery,
    and is flawed by ONE OR MORE of the following weaknesses: develops a
    point of view on the issue that is vague or seriously limited, and
    demonstrates weak critical thinking, providing inappropriate or
    insufficient examples, reasons, or other evidence to support its
    position; is poorly organized and/or focused, or demonstrates
    serious problems with coherence or progression of ideas; displays
    very little facility in the use of language, using very limited
    vocabulary or incorrect word choice; demonstrates frequent problems
    in sentence structure; contains errors in grammar, usage, and
    mechanics so serious that meaning is somewhat obscured.

    SCORE OF 1: An essay in this category demonstrates very little or no mastery, and is severely flawed by ONE OR MORE of the following weaknesses: develops no viable point of view on the issue, or provides little or no evidence to support its position; is disorganized or unfocused, resulting in a disjointed or incoherent essay; displays fundamental errors in vocabulary; demonstrates severe flaws in sentence structure; contains pervasive errors in grammar, usage, or mechanics that persistently interfere with meaning.
    """
    prompt = f"""
    You are an expert essay grader. Your task is to rate the following essay on a scale of 1 (minimum) to 6 (maximum) based on the rubric. The distance between each grade (e.g., 1-2, 3-4, 4-5) should be considered equal. Provide only the score as a single integer (1, 2, 3, 4, 5, or 6).

    Rubric:
    {rubric}

    Essay:
    {essay}

    Holistic score based on attached rubric (1-6): 
    """
    return prompt


def make_request(filename, essay):
    prompt = create_prompt(essay)
    return [Request(
        custom_id=filename.split(".")[0]+"-1",
        params=MessageCreateParamsNonStreaming(
            model="claude-3-7-sonnet-20250219", 
            max_tokens=2,
            temperature=0.7,
            messages=[
                {"role": "user", "content": prompt}
            ]
        )
    ),
    Request(
        custom_id=filename.split(".")[0]+"-2",
        params=MessageCreateParamsNonStreaming(
            model="claude-3-7-sonnet-20250219", 
            max_tokens=2,
            temperature=0.7,
            messages=[
                {"role": "user", "content": prompt}
            ]
        )
    )]


requests = [item for filename, essay in essays.items() for item in make_request(filename, essay)]
message_batch = client.messages.batches.create(requests=requests)
print(message_batch)

