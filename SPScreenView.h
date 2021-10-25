
//
// SPScreenView.h
// Snowplow
//
// Copyright (c) 2013-2021 Snowplow Analytics Ltd. All rights reserved.
//
// This program is licensed to you under the Apache License Version 2.0,
// and you may not use this file except in compliance with the Apache License
// Version 2.0. You may obtain a copy of the Apache License Version 2.0 at
// http://www.apache.org/licenses/LICENSE-2.0.
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the Apache License Version 2.0 is distributed on
// an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
// express or implied. See the Apache License Version 2.0 for the specific
// language governing permissions and limitations there under.
//
// Copyright: Copyright © 2021 Snowplow Analytics.
// License: Apache License Version 2.0
//

#import "SPEventBase.h"
#import "SPSelfDescribingJson.h"

NS_ASSUME_NONNULL_BEGIN

@interface SPScreenView : SPSelfDescribingAbstract
extern NSString * const kSPScreenViewSchema;

extern NSString * const kSPScreenViewParamName;

extern NSString * const kSPScreenViewParamType;

extern NSString * const kSPScreenViewParamId;

extern NSString * const kSPScreenViewParamPreviousName;

extern NSString * const kSPScreenViewParamPreviousId;

extern NSString * const kSPScreenViewParamPreviousType;

extern NSString * const kSPScreenViewParamTransitionType;



/// The name of the screen viewed.
@property (nonatomic, nonnull, readonly) NSString *name;

/// The type of screen that was viewed e.g feed / carousel.
@property (nonatomic, nonnull, readonly) NSString *type;

/// An ID from the associated screenview event.
@property (nonatomic, nonnull, readonly) NSString *id;

/// The name of the previous screen.
@property (nonatomic, nullable) NSString *previousName;

/// A screenview ID of the previous screenview.
@property (nonatomic, nonnull, readonly) NSString *previousId;

/// The screen type of the previous screenview.
@property (nonatomic, nonnull, readonly) NSString *previousType;

/// The type of transition that led to the screen being viewed.
@property (nonatomic, nonnull, readonly) NSString *transitionType;


- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name Type:(NSString *)type Id:(NSString *)id PreviousId:(NSString *)previousId PreviousType:(NSString *)previousType TransitionType:(NSString *)transitionType ;


SP_BUILDER_DECLARE_NULLABLE(NSString *, previousName)


@end

NS_ASSUME_NONNULL_END
